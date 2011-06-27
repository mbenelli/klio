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
(##namespace ("fecthwrite#" OK OPCODE-WRITE OPCODE-FETCH make-response-header))


(define buffer (make-u8vector 1024))
(define request (make-u8vector 16))

(define buffer-mutex (make-mutex))
(define request-mutex (make-mutex))

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
   (case opcode
     ((OPCODE-WRITE) (write org-id db offset len (subu8vector req 16) p))
     ((OPCODE-FECTH) (fetch org-id db offset len p))
     (else (raise "Unknown opcode")))))




(define (write org-id db offset len data #!optional (p (current-output-port)))
  (with-input-from-u8vector
    data
    (lambda ()
      (mutex-lock! buffer-mutex)
      (read-subu8vector buffer offset (+ offset len))
      (mutex-unlock! buffer-mutex)))
  (with-output-to-port
    p
    (lambda ()
      (write-sub8vector (make-response-header OK) 0 16))))


(define (fetch org-id db offset len #!optional (p (current-output-port)))
  (with-output-to-port
    p
    (lambda ()
      (write-subu8vector (make-response-header OK) 0 16)
      (mutex-lock! buffer-mutex)
      (write-subu8vector buffer offset (+ offset len))
      (mutex-unlock! buffer-mutex))))


(define (srv s)
  (let* ((p (read s)))
    (mutex-lock! request-mutex)
    (read-sub8vector request 0 16 p)
    (serve-request request p)
    (mutex-unlock! request-mutex)
    (fetch-srv s)))

(define (simulator-start fetch-port write-port)
  (let ((fp (open-tcp-server fetch-port))
	(wp (open-tcp-server write-port))
	(ft (make-thread (lambda () (srv fp))))
	(wt (make-thread (lambda () (srv wp)))))
    (thread-start! ft)
    (thread-start! wt)))

