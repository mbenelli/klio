;; fwsim.scm - Simulator for fetch-write protocol
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Right Reserved.
;;
;; Author: Marco Benelli <mbenelli@yahoo.com>
;;
;; The fecth-write protocol is used to communicate with Siemens S5/S7 plcs.

(##namespace ("fwsim#"))
(##include "~~lib/gambit#.scm")
(##namespace ("fecthwrite#" OPCODE-WRITE OPCODE-FETCH))


(define buffer (make-u8vector 1024))

(define (parse-request req)
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
     ((OPCODE-WRITE) (write org-id db offset len (subu8vector req 16)))
     ((OPCODE-FECTH) (fecth org-id db offset len))
     (else (raise "Unknown opcode")))))


(define (write org-id db offset len data)
  (with-input-from-u8vector
    data
    (lambda ()
      (read-subu8vector buffer offset (+ offset len)))))


(define (fetch org-id db offset len #!optional (p (current-output-port)))
  (with-output-to-port
    p
    (lambda ()
      (write-subu8vector buffer offset (+ offset len)))))



