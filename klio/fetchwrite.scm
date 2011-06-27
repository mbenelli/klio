;; fetchwrite.scm - Fetch-Write protocol implementation.
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Right Reserved.
;;
;; Author: Marco Benelli <mbenelli@yahoo.com>
;;
;; The fecth-write protocol is used to communicate with Siemens S5/S7 plcs.

(##namespace ("fetchwrite#"))


;; ORG types

(define DB    1)    ; Main memory data block
(define M     2)    ; Flag area
(define I     3)    ; Inputs
(define Q     4)    ; Outputs
(define PI-PQ 5)    ; Analog I/O
(define C     6)    ; Counter cells
(define T     7)    ; Timer cells


;; Opcodes

(define OPCODE-WRITE 3)
(define OPCODE-FETCH 5)


;; Error codes

(define OK                   0)
(define ERROR               -1)
(define ERROR-INVALID-PARAM -2)
(define ERROR-CONNECTION    -3)
(define ERROR-TIMEOUT       -4)
(define ERROR-COMMUNICATION -5)
(define ERROR-BUFFER        -6)
(define ERROR-SEND          -7)
(define ERROR-RECV          -8)


;; Request header
;;
;; Byte#    Field Name               Value
;;
;;  0       System id                #\S
;;  1       System id                #\5
;;  2       Header Length            16d
;;  3       ID Op Code               1
;;  4       Op Code Length           3
;;  5       Op Code
;;  6       ORG Field                3
;;  7       Org Field Lenght         8
;;  8       ORG ID
;;  9       DB Number
;; 10       Start Address high
;; 11       Start Address low
;; 12       Number of words high
;; 13       Number of words low
;; 14       Empty Field              FFh
;; 15       Empty Field Size         2
;; 16       Data up to 64k bytes

(define (make-request-header opcode org-id db offset len)
  (u8vector
    (char->integer #\S)
    (char->integer #\5)
    16
    1
    3
    3
    opcode
    8
    org-id
    db
    (extract-bit-field 8 8 offset)
    (extract-bit-field 8 0 offset)
    (extract-bit-field 8 8 len)
    (extract-bit-field 8 0 len)))



;; Response header
;;
;; Byte#    Field Name               Value
;;
;;  0       System ID                \#S
;;  1       System ID                \#5
;;  2       Header Length            16d
;;  3       ID OP Code               1
;;  4       Op Code Length           3
;;  5       Op Code Length           4
;;  6       Ack Field                OFh
;;  7       S Field Lenght           3
;;  8       Error Number
;;  9       Empty Field              FFh
;; 10       length Empty Field       7
;; 11   |
;; 12   |
;; 13   |-->  Free
;; 14   |
;; 15   |

(define (make-response-header errno)
  (u8vector
    (char->integer \#S)
    (char->integer \#5)
    16
    1
    3
    4
    #x0f
    3
    errno
    #xff
    7
    0
    0
    0
    0
    0))


(define (write-db db offset len data #!optional (p (current-output-port)))
  (let ((req-header (make-request-header OPCODE-WRITE DB db offset len))
        (res-header (make-u8vector 16)))
    (with-output-to-port p
      (lambda ()
        (write-subu8vector req-header 0 (u8vector-length req-header))
        (write-subu8vector data 0 (u8vector-length data))))
    (force-output p)
    (with-input-from-port p
      (lambda ()
        (read-subu8vector res-header 0 (u8vector-length res-header))))))

(define (fetch-db db offset len #!optional (p (current-output-port)))
  (let ((req-header (make-request-header OPCODE-FETCH DB db offset len))
        (res-header (make-u8vector 16))
        (res (make-u8vector len)))
    (with-output-to-port p
      (lambda ()
        (write-subu8vector req-header 0 (u8vector-length req-header))))
    (force-output p)
    (with-input-from-port p
      (lambda ()
        (read-sub8vector res-header 0 (u8vector-length res-header))
        (read-sub8vector res 0 len)))
    res))
