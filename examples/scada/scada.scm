;; scada.scm - An example of a scada system
;;
;; Copyright (c) 2011-2012 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

;; Warning: this example isn't working yet.
;;
;; TODO:
;;
;; - complete responses

(##namespace ("scada#"))
(##include "~~lib/gambit#.scm")
(##include "~/.klio/prelude#.scm")


(##namespace
  ("lists#" lset-difference)
  ("uri#" uri-path uri-query)
  ("http-srv#" reply current-request request-connection request-query
   request-attributes request-uri see-other response-date)
  ("json#" json-write json-read)
  ("sqlite3#" sqlite3)
  ("binary-io#" read-ieee-float32)
  ("fetchwrite#" fetch/apply write-db)
  ("kws#" kws get-static get-file *server-root*)
  ("sessions#" make-session-builder valid-session session-id session-user
   start-session-manager stop-session-manager))


                                        ; == Params ==


                                        ; Fetch-write parameters

(define datasource-address (make-parameter "localhost"))
(define fetch-port-number (make-parameter 2000))
(define write-port-number (make-parameter 2001))


                                        ; Read data each ACQUISITION-PERIOD
                                        ; seconds.

(define acquisition-period (make-parameter 2))


                                        ; Write on db each SAVE-PERIOD times
                                        ; (multiply for ACQUISITION-PERIOD).

(define save-period (make-parameter 5))


                                        ; Number of channels [1,6]

(define n-channels (make-parameter 6))


                                        ; Utilities

(define (read-ieee-f32vector n port)
  (let ((res (make-f32vector n)))
    (do ((i 0 (+ i 1)))
      ((= i n) res)
      (f32vector-set! res i (read-ieee-float32 port 'big-endian)))))

(define (current-seconds)
  (inexact->exact (round (time->seconds (current-time)))))

(define (post-value name)
  (and-let* ((query (request-query (current-request)))
             (kv-pair (assoc name query)))
    (cdr kv-pair)))

(define (get-value name)
  (and-let* ((query (uri-query (request-uri (current-request))))
             (kv-pair (assoc name query)))
    (cdr kv-pair)))

(define (success)
  (json-write (list->table '((succeeded . #t)))))

(define (failure #!optional (msg ""))
  (json-write (list->table `((succeeded . #f) (msg . ,msg)))))


                                        ; Fetch Write

(define fetch-port
  (open-tcp-client
    `(server-address: ,(datasource-address) port-number: ,(fetch-port-number))))

(define write-port
  (open-tcp-client
    `(server-address: ,(datasource-address) port-number: ,(write-port-number))))


                                        ; == Data map ==

(define measures (make-f32vector 32))
(define alarms (make-u8vector 10))
(define enablings (make-u8vector 16))   ; channels, commands, ded, ded,
                                        ; aux, aux, vacuum, vacuum
                                        ; channels, commands, ded, ded,
                                        ; aux, aux, vacuum, vacuum

(define prms (make-f32vector 24))       ; final setpoint, current setpoint
                                        ; slope (8x)
(define timestamp #f)

(define (get-measure i)
  (f32vector-ref measures i))

(define (get-active-channels)
  (let ((channels-mask (u8vector-ref enablings 8)))
    (map (lambda (i) (bit-set? i channels-mask)) '(0 1 2 3 4 5 6 7))))

(define (get-dedicated-contacts)
  (u8vector-ref enablings 10)) ; 11?

(define (get-auxiliary-contacts)
  (u8vector-ref enablings 12)) ; 13?

(define (get-final-setpoint i)
  (f32vector-ref prms (* i 3)))

(define (get-current-setpoint i)
  (f32vector-ref prms (+ 1 (* i 3))))

(define (get-slope i)
  (f32vector-ref prms (+ 2 (* i 3))))

                                        ; -- Alarms --

(define active-alarms '())

(define (init-active-alarms-from-db)
  (with-mutex alarms-mutex
    (set! active-alarms
      (fold/query
        (lambda (seed . cols)
          (values #t (cons (car cols) seed)))
        '()
        "SELECT id FROM alarms WHERE end IS NULL"))))

(define alarms-mutex (make-mutex))

                                        ; List of alarm identifiers.
                                        ; Assume that alarms are represented
                                        ; as bits.
(define alarmlist
  '#(#(a00 a01 a02 a03 a04 a05 a06 a07)
      #(a10 a11 a12 a13 a14 a15 a16 a17)))

                                        ; Check active alarms
(define (check-alarms alarms)
  (let ((active-alarms '()))
    (do ((i 0 (+ i 1)))
        ((= i (u8vector-length alarms)) active-alarms)
      (if (not (zero? (u8vector-ref alarms i)))
          (do ((j 0 (+ j 1)))
              ((= j 8) 'done)
            (if (bit-set? j (u8vector-ref alarms i))
                (push! (vector-ref (vector-ref alarmlist 1) j)
                  active-alarms)))))))


(define (update-alarms)
  (let* ((current-alarms (check-alarms alarms))
         (turned-on (lset-difference eq? current-alarms active-alarms))
         (turned-off (lset-difference eq? active-alarms current-alarms)))
    (pp "checking alarms")
    (pp active-alarms)
    (pp turned-on)
    (pp turned-off)
    (with-mutex alarms-mutex
      (set! active-alarms current-alarms))
    (save-alarms turned-on turned-off)
    ))

(define (save-alarms on off)
  (unless (null? off)
    (fold/query
      (lambda (seed . cols) (values #t seed))
      '()
      (with-output-to-string
        "UPDATE alarms SET end=strftime('%s','now','localtime')"
        (lambda ()
          (print " WHERE end IS NULL AND (")
          (print
            (intersperse
              (map
                (lambda (x)
                  (with-output-to-string
                    "id="
                    (lambda () (print #\' x #\'))))
                off)
              " OR "))
          (print ")")))))
  (unless (null? on)
    (for-each
      (lambda (x)
        (fold/query
          (lambda (seed . cols) (values #t seed))
          '()
          (with-output-to-string
            "INSERT INTO alarms (id, start, end) VALUES ("
            (lambda ()
              (print #\' x #\' ", strftime('%s','now','localtime'), NULL)")))))
      on)))



                                        ; == FetchWrite communications ==

(define fw-mutex (make-mutex))
(define db-mutex (make-mutex))

(define (read-db p)
  (set! measures (read-ieee-f32vector 32 p))
  (read-subu8vector alarms 0 10 p)
  (read-subu8vector enablings 0 16 p)
  (set! prms (read-ieee-f32vector 24 p))
  (set! timestamp (time->seconds (current-time))))

(define (save-current-state)
  (let ((db (make-dbhandler (string-append (*server-root*) "/" (dbname))))
	(q (with-output-to-string "INSERT INTO history VALUES ("
	    (lambda ()
	     (print (current-seconds) ",")
	     (do ((i 0 (+ i 1)))
	      ((= i (n-channels)) '())
	      (print (get-measure i) ",")
	      (print (get-final-setpoint i) ",")
	      (print (get-current-setpoint i) ",")
	      (print (get-slope i) ","))
	     (print (get-dedicated-contacts) ",")
	     (print (get-auxiliary-contacts) ")")))))
    ((dbhandler-run db) values "" q)
    ((dbhandler-close db))))


(define update
  (let ((counter 0))
    (lambda ()
      (with-mutex fw-mutex
        (fetch/apply 62 0 125 read-db fetch-port))
      (update-alarms)
      (set! counter (+ 1 counter))
      (when (= counter (save-period))
	(with-mutex db-mutex
	  (save-current-state))
	(set! counter 0)))))


(define update-thread
  (make-thread
    (lambda ()
      (let ((start (time->seconds (current-time)))
	    (dt (acquisition-period)))
	(let loop ((x dt))
	  (thread-sleep! (seconds->time (+ x start)))
	  (update)
	  (loop (+ x dt)))))))


                                        ; Assume that most significant values
                                        ; are the leftmost bits.

(define (write-contacts ded aux)
  (write-db 62 74 2 (u8vector ded 0 aux 0) write-port)) ; 501

(define (write-setpoints-and-slopes data)
  (write-db 62 75 32 ; 504
	    (with-output-to-u8vector (make-u8vector 64)
	      (lambda ()
		(for-each write-ieee-float32 data)))
	    write-port))




                                        ; == Database handling ==

(define dbname (make-parameter "data.sqlite"))
(define dbfilename (string-append (*server-root*) "/" (dbname)))

(define-type dbhandler
  constructor: %make-dbhandler
  run
  close)

(define (make-dbhandler dbpath)
  (call-with-values
    (lambda () (sqlite3 dbpath))
    (lambda (hdl close)
      (%make-dbhandler hdl close))))

(define (fold/query fn seed query)
  (let* ((db (make-dbhandler dbfilename))
         (res ((dbhandler-run db) fn seed query)))
    ((dbhandler-close db))
    res))

(define (display-row seed . cols)
  (println (map (lambda (x) (cons x #\space)) cols))
  (values #t ""))



                                        ; == Pages ==

(define pages (make-table test: string=?))

                                        ; == Dispatcher ==

(define (dispatch)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (cond
      ((string=? path "/") (get-file "/index.html" '()))
      ((table-ref pages path #f)
       => (lambda (x) (reply x '(("Content-Type" . "application/json")))))
      (else (get-static)))))

