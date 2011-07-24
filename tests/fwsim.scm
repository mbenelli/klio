;; fwsim.scm - Simulator for fetch-write protocol
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Right Reserved.
;;
;; Author: Marco Benelli <mbenelli@yahoo.com>
;;
;; The fecth-write protocol is used to communicate with Siemens S5/S7 plcs.
;;
;;
;; TODO: check mutex

(##include "~~lib/gambit#.scm")
(##namespace ("fetchwrite#" OK OPCODE-WRITE OPCODE-FETCH make-response-header))


(define buffer (make-u8vector 1024))


(define sample-data
  '#u8(

       ; Measures

       66 242 4 108
       67 159 74 131
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       64 224 0 0
       65 166 149 142
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       65 1 213 9
       0 0 0 0
       66 242 4 108
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       0 0 0 0
       66 242 4 119
       0 0 0 0
       0 0 0 0
       65 152 0 0
       0 0 0 0
       0 0 0 0

       ; Alarms 

       0 0 0 0 0 0 0 0 0 0

       ; misc

       9 0 0 141 0 0 11 232 0 0 0 0 0 0 0 0 67 22 0 0 0 0 0 0 64 64 0 0 66 72
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0))

(define buffer-mutex (make-mutex))

(define (serve-request req p)
 (let* ((opcode (u8vector-ref req 5))
        (org-id (u8vector-ref req 8))
        (db (u8vector-ref req 9))
        (offset-high (u8vector-ref req 10))
        (offset-low (u8vector-ref req 11))
	(offset (bitwise-ior (arithmetic-shift offset-high 8) offset-low))
        (len-high (u8vector-ref req 12))
        (len-low (u8vector-ref req 13))
	(len (bitwise-ior (arithmetic-shift len-high 8) len-low)))
   (cond
     ((eqv? opcode OPCODE-WRITE)
      (db-write org-id db offset len p))
     ((eqv? opcode OPCODE-FETCH)
      (fetch org-id db offset len p))
     (else (raise "Unknown opcode")))))




(define (db-write org-id db offset len
                  #!optional (p (current-output-port)))
  (let ((data (make-u8vector len)))
    (read-subu8vector data 0 len p)
    (with-input-from-u8vector
      data
      (lambda ()
        ;(mutex-lock! buffer-mutex #f #f)
        (read-subu8vector sample-data offset (+ offset len))
        ;(mutex-unlock! buffer-mutex)
        ))
    (write-subu8vector (make-response-header OK) 0 16 p)
    (force-output p)))


(define (fetch org-id db offset len #!optional (p (current-output-port)))
  (write-subu8vector (make-response-header OK) 0 16 p)
  (mutex-lock! buffer-mutex #f #f)
  (write-subu8vector sample-data offset (+ offset len) p)
  (force-output p)
  (mutex-unlock! buffer-mutex))


(define (serve-connection p)
  (let ((request (make-u8vector 16)))
    (print (time->seconds (current-time)) " - Serving request ...")
    (read-subu8vector request 0 16 p)
    (serve-request request p)
    (println "done.")
    (serve-connection p)))

(define (srv s)
  (let ((p (read s)))
    (thread-start!
      (make-thread
	(lambda ()
	  (serve-connection p))))
    (srv s)))

(define (simulator-start fetch-port write-port)
  (let* ((fp (open-tcp-server fetch-port))
         (wp (open-tcp-server write-port))
         (ft (make-thread (lambda () (srv fp))))
         (wt (make-thread (lambda () (srv wp)))))
    (thread-start! ft)
    (thread-start! wt)
    (if (char=? (read-char) #\q)
	(exit))))

