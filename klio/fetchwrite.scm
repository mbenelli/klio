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
;;  5       Op Code Lenght           3
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

(define request-header
  (u8vector
    (char->integer \#S)
    (char->integer \#5)
    16
    1
    3
    3
    3
    8
    0
    0
    0
    0
    0
    0
    255
    2))

(define make-request)

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


(define (write-db db offset length data #!optional (port (current-output-port)))
  )