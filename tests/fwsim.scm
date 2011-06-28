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

(##namespace ("fwsim#"))
(##include "~~lib/gambit#.scm")
(##namespace ("fetchwrite#" OK OPCODE-WRITE OPCODE-FETCH make-response-header))


(define buffer (make-u8vector 1024))

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
        (read-subu8vector buffer offset (+ offset len))
        ;(mutex-unlock! buffer-mutex)
        ))
    (write-subu8vector (make-response-header OK) 0 16 p)
    (force-output p)))


(define (fetch org-id db offset len #!optional (p (current-output-port)))
  (write-subu8vector (make-response-header OK) 0 16 p)
  ;(mutex-lock! buffer-mutex #f #f)
  (write-subu8vector buffer offset (+ offset len) p)
  (force-output p)
  ;(mutex-unlock! buffer-mutex)
  )


(define (srv s)
  (let* ((p (read s))
         (request (make-u8vector 16)))
    (read-subu8vector request 0 16 p)
    (serve-request request p)
    (srv s)))

(define (simulator-start fetch-port write-port)
  (let* ((fp (open-tcp-server fetch-port))
         (wp (open-tcp-server write-port))
         (ft (make-thread (lambda () (srv fp))))
         (wt (make-thread (lambda () (srv wp)))))
    (thread-start! ft)
    (thread-start! wt)))

