; kws-fw.scm - fetch-write test for klio web server
;
; Copyright (c) 2008-2010 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.

(##namespace ("kws-fw#"))
(##include "~~lib/gambit#.scm")
(##include "../klio/prelude#.scm")
(##namespace
  ("http#" reply current-request request-uri uri-query
    uri-path)
  ("binary-io#" read-ieee-float32)
  ("fetchwrite#" fetch/apply)
  ("json#" json-write)
  ("kws#" get-static *server-root*))


; Parameters

(define datasource-address (make-parameter "localhost"))
(define fetch-port-number (make-parameter 2000))
(define write-port-number (make-parameter 2001))


; Utilities

(define (read-ieee-f32vector n port)
  (let ((res (make-f32vector n)))
    (do ((i 0 (+ i 1)))
      ((= i n) res)
      (f32vector-set! res i (read-ieee-float32 port 'big-endian)))))


; Fetch handling

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

(define measures-mutex (make-mutex))

(define (read-db p)
  (mutex-lock! measures-mutex #f #f)
  (set! measures (read-ieee-f32vector 32 p))
  (mutex-unlock! measures-mutex)
  (read-subu8vector alarms 0 10 p)
  (read-subu8vector enablings 0 16 p)
  (set! prms (read-ieee-f32vector 24 p))
  (set! timestamp (time->seconds (current-time))))


(define (update)
  (fetch/apply 62 0 250 read-db fetch-port))


(define update-thread
  (make-thread
    (lambda ()
      (let ((start (time->seconds (current-time)))
	    (dt 2))
	(let loop ((x dt))
	  (thread-sleep! (seconds->time (+ x start)))
	  (update)
	  (loop (+ x dt)))))))


;;; Pages

(define pages (make-table test: string=?))

(table-set! pages "/status"
            (lambda ()
	      (mutex-lock! measures-mutex #f #f)
              (json-write
                (list->table
                  `((user . "user00")
                    (time . ,timestamp)
                    (status . "running")
                    (channels . ,(list->table
                                   `(("ch0" . ,(f32vector-ref measures 0))
                                     ("ch1" . ,(f32vector-ref measures 1))
                                     ("ch2" . ,(f32vector-ref measures 2))
                                     ("ch3" . ,(f32vector-ref measures 3))
                                     ("ch4" . ,(f32vector-ref measures 4))
                                     ("ch5" . ,(f32vector-ref measures 5)))))
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
                                      (severity . 2))))))))
	      (mutex-unlock! measures-mutex)))

(define (dispatcher)
  (let ((path (uri-path (request-uri (current-request)))))
    (cond
      ((table-ref pages path #f)
       => (lambda (x)
            (reply x)))
      (else (get-static)))))

