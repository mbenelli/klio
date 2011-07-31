; demo01.scm - An example of a scada system
;
; Copyright (c) 2008-2010 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.

; Warning: this example isn't working yet.
;
; TODO:
;
; - test database inserts
; - dynamically select measure to be saved
; - check alarms
; - insert parameters into database
; - add function to write parameters and contacts

(##namespace ("demo01#"))
(##include "~~lib/gambit#.scm")
(##include "~/.klio/prelude#.scm")


(##namespace
  ("http#" reply current-request request-connection request-query
   request-attributes request-uri uri-path uri-query see-other
   response-date)
  ("json#" json-write json-read)
  ("sqlite3#" sqlite3)
  ("fetchwrite#" fetch/apply)
  ("kws#" kws get-static get-file *server-root*)
  ("sessions#" make-session-builder valid-session session-id session-user
   start-session-manager stop-session-manager))


; Params


; Write on db each SAVE-PERIOD times.

(define save-period (make-parameter 5)) 

; Utilities

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


; Data map

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
  (f32vector-ref mesures i))

(define (get-active-channels)
  (let ((channels-mask (u8vector-ref enablings 8)))
    (map (lambda (i) (bit-set? i channels-mask)) '(0 1 2 3 4 5 6 7))))

(define (get-dedicated-contacts)
  (u8vector-ref enablings 12)) ; 13?

(define (get-auxiliary-contacts)
  (u8vector-ref enablings 14)) ; 15?

(define (get-final-setpoint i)
  (f32vector-ref prms (* i 3)))

(define (get-current-setpoint i)
  (f32vector-ref prms (+ 1 (* i 3))))

(define (get-slope i)
  (f32vector-ref prms (+ 2 (* i 3))))

(define fw-mutex (make-mutex))
(define db-mutex (make-mutex))

(define (read-db p)
  (set! measures (read-ieee-f32vector 32 p))
  (read-subu8vector alarms 0 10 p)
  (read-subu8vector enablings 0 16 p)
  (set! prms (read-ieee-f32vector 24 p))
  (set! timestamp (time->seconds (current-time))))

(define (save-current-state)
  (let ((db (make-db-handler (string-append (*server-root*) "/" (dbname))))
	(q (with-output-to-string "INSERT INTO measures VALUES ("
	     (do ((i 0 (+ i 1)))
	       ((= i 6) '())
	       (print (get-measure i) ","))
	     (print (get-dedicated-contacts) ",")
	     (print (get-auxiliary-contacts) ")"))))
    ((db-handler-run db) values "" q)
    ((db-handler-close db))))


(define (update)
  (mutex-lock! fw-mutex #f #f)
  (fetch/apply 62 0 250 read-db fetch-port)
  (mutex-unlock! fw-mutex))

(define update
  (let ((counter 0))
    (lambda ()
      (with-mutex fw-mutex
        (fetch/apply 62 0 250 read-db fetch-port))
      (set! counter (+ 1 counter))
      (when (= counter (save-period))
	(save-current-state)
	(set! counter 0)))))


(define update-thread
  (make-thread
    (lambda ()
      (let ((start (time->seconds (current-time)))
	    (dt 2))
	(let loop ((x dt))
	  (thread-sleep! (seconds->time (+ x start)))
	  (update)
	  (loop (+ x dt)))))))




;;; Database handling

(define dbname (make-parameter "plantmaster.sqlite"))

(define-type dbhandler
  constructor: %make-dbhandler
  run
  close)

(define (make-dbhandler dbpath)
  (call-with-values
    (lambda () (sqlite3 dbpath))
    (lambda (hdl close)
      (%make-dbhandler hdl close))))

(define (display-row seed . cols)
  (println (map (lambda (x) (cons x #\space)) cols))
  (values #t ""))


;;; Pages

(define pages (make-table test: string=?))

; REMOVEME

(table-set! pages "/select"
  (lambda ()
    (and-let* ((q (uri-query (request-uri (current-request))))
	       (t (assoc "table" q))
	       (table (cdr t)))
      (let ((db (make-dbhandler (string-append (*server-root*) "/" (dbname)))))
        ((dbhandler-run db) display-row ""
         (string-append "SELECT * FROM " table))
        ((dbhandler-close db))))))

(table-set! pages "/status"
            (lambda ()
              (json-write
                (list->table
                  `((user . "user00")
                    (time . ,(response-date))
                    (status . "running")
                    (channels . ,(list->table
                                   '(("ch0" . 37)
                                     ("ch1" . 1)
                                     ("ch2" . 1)
                                     ("ch3" . 50)
                                     ("ch4" . 1)
                                     ("ch5" . 1))))
                    (contacts . ,(list->table
                                   '(("dedicated" . #b00000000)
                                     ("auxiliaries" . #b00000000))))
                    (notifies . (,(list->table
                                    '((msg . "Notify 1")
                                      (severity . 1)))
                                 ,(list->table
                                    '((msg . "Notify 2")
                                      (severity . 3)))
                                 ,(list->table
                                    '((msg . "Notify 3")
                                      (severity . 2))))))))))


;;; Monitor


(table-set! pages "/monitor/overview"
            (lambda ()
              (json-write
                (list->table
                  `((ch0 . ,(list->table
                              `((value . 23.2)
                                (setpoints . ,(list->table
                                                `((prev . 5.0)
                                                  (next . 30.0))))
                                (gradient . 0)
                                (trend . 1)
                                (settings . "uncontrolled")
                                (wait_duration . #t)
                                (wait_setpoint . #f))))
                    (ch1 . ,(list->table
                              `((value . 10.0)
                                (setpoints . ,(list->table
                                                `((prev . 5.0)
                                                  (next . 15.0))))
                                (gradient . 0)
                                (trend . 1)
                                (settings . "max_speed")
                                (wait_duration . #t)
                                (wait_setpoint . #f))))
                    (ch2 . ,(list->table
                              `((value . 12.0)
                                (setpoints . ,(list->table
                                                `((prev . 20.0)
                                                  (next . 10.0))))
                                (gradient . -1.3)
                                (trend . -1)
                                (settings . "controlled")
                                (wait_duration . #f)
                                (wait_setpoint . #f))))
                    (ch3 . ,(list->table
                              `((value . 1.4)
                                (setpoints . ,(list->table
                                                `((prev . 0.0)
                                                  (next . 2.0))))
                                (gradient . 0)
                                (trend . 1)
                                (settings . "maintenance")
                                (wait_duration . #f)
                                (wait_setpoint . #t))))
                    (ch4 . ,(list->table
                              `((value . 4.5)
                                (setpoints . ,(list->table
                                                `((prev . 8.5)
                                                  (next . 2.0))))
                                (gradient . 0)
                                (trend . -1)
                                (settings . "max_speed")
                                (wait_duration . #t)
                                (wait_setpoint . #t))))
                    (ch5 . ,(list->table
                              `((value . 11.8)
                                (setpoints . ,(list->table
                                                `((prev . 5.4)
                                                  (next . 15.3))))
                                (gradient . 0)
                                (trend . -1)
                                (settings . "uncontrolled")
                                (wait_duration . #t)
                                (wait_setpoint . #f)))))))))


(table-set! pages "/monitor/graphs/channels"
            (lambda ()
              (json-write
                (list->table
                  `((available_channels . (temperature pressure humidity))
                    (active_channels . (temperature pressure)))))))

(table-set! pages "/monitor/graphs/points"
            (lambda ()
              (json-write
                (list->table
                  `((v0 . ((0 0) (1 1) (2 2) (3 3)))
                    (v1 . ((0 0) (1 1) (2 4) (3 9))))))))


;;; Test - Manual


(define test-prms
  (list->table
    `((channels . ,(list->table
                     `((temperature . ,(list->table
                                         `((active . #t)
                                           (current . 30)
                                           (target . 40)
                                           (ramp . #t))))
                       (pressure . ,(list->table
                                      `((active . #t)
                                        (current . 20)
                                        (target . 30)
                                        (ramp . #f)))))))
      (contacts . ,(list->table
                     `((dedicated . #x00000000)
                       (auxiliaries . #x00000000)))))))


(table-set! pages "/test/channels"
            (lambda ()
              (json-write test-prms)))

(table-set! pages "/test/run"
            (lambda ()
              (let ((prms (post-value "prms")))
                (success))))

(table-set! pages "/test/delayed"
            (lambda ()
              (let ((prms (post-value "prms"))
                    (start (post-value "starttime")))
                (success))))

(table-set! pages "/test/countdown"
            (lambda ()
              (let ((prms (post-value "prms"))
                    (interval (post-value "interval")))
                (success))))

;;; Test - Program

(table-set! pages "/cycles/archieve"
            (lambda ()
              (json-write
                (list
                  (list->table
                    `((name . "cycle0")
                      (category . "test")
                      (last-run . ,(response-date))
                      (creation . ,(response-date))
                      (n_channels . 2)
                      (n_contacts . 16)
                      (length . 20)))
                  (list->table
                    `((name . "cycle1")
                      (category . "test")
                      (last-run . ,(response-date))
                      (creation . ,(response-date))
                      (n_channels . 3)
                      (n_contacts . 8)
                      (length . 24)))))))

(define cycle
  (list
    (list->table
      `((timestamp . ,(response-date))
        (channels . ,(list
                       (list->table
                         `((setpoint . 10)
                           (ramp . #f)
                           (gradient . #f)))
                       (list->table
                         `((setpoint . 20)
                           (ramp . #f)
                           (gradient . #f)))))
        (contacts . ,(list->table
                       `((dedicated . #x00000000)
                         (auxiliaries . #x00000000))))))
    (list->table
      `((timestamp . ,(response-date))
        (channels . ,(list
                       (list->table
                         `((setpoint . 10)
                           (ramp . #f)
                           (gradient . #f)))
                       (list->table
                         `((setpoint . 20)
                           (ramp . #f)
                           (gradient . #f)))))
        (contacts . ,(list->table
                       `((dedicated . #x00000000)
                         (auxiliaries . #x00000000))))))))

(define alarms
  (list->table
    `((temperature . (0 100))
      (pressure . (0 100)))))


(table-set! pages "/cycles/load"
            (lambda ()
              (let ((name (get-value "name")))
                (json-write
                  (list->table
                    `((cycle . ,cycle) (alarms . ,alarms)))))))

(table-set! pages "/cycles/save"
            (lambda ()
              (let ((name (post-value "name"))
                    (category (post-value "category"))
                    (cycle (post-value "cycle"))
                    (alarms (post-value "alarms")))
                (success))))

(table-set! pages "/cycles/run"
            (lambda ()
              (let ((cycle (post-value "cycle"))
                    (alarms (post-value "alarms")))
                (success))))


;;; Archive


(table-set! pages "/archieve/test"
            (lambda ()
              (json-write
                (list
                  (list->table
                    `((name . "test0")
                      (start . ,(response-date))
                      (end . ,(response-date))
                      (length . 0)))
                  (list->table
                    `((name . "test1")
                      (start . ,(response-date))
                      (end . ,(response-date))
                      (length . 0)))))))

(table-set! pages "/archieve/notifies"
            (lambda ()
              (json-write
                (list
                  (list->table
                    `((title . "notify0")
                      (description . "buh!")
                      (timestamp . ,(response-date))
                      (level . 1)))
                  (list->table
                    `((title . "notify1")
                      (description . "buh!")
                      (timestamp . ,(response-date))
                      (level . 1)))))))
;;;

(define (dispatch)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (cond
      ((string=? path "/") (get-file "/index.html" '()))
      ((table-ref pages path #f)
       => (lambda (x) (reply x '(("Content-Type" . "application/json")))))
      (else (get-static)))))

