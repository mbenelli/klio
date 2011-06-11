;; modbus.scm - Modbus protocol implementation.
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Right Reserved.
;;
;; Author: Marco Benelli <mbenelli@yahoo.com>
;;

;; Actually only modbus-tcp is implemented.

(##namespace ("modbus#"))
(##include "~~lib/gambit#.scm")

; Allocation-free version

(define send-buff-size (make-parameter 12))
(define *send-buffer* (make-u8vector (send-buff-size)))

(define recv-buff-size (make-parameter 2048))
(define *recv-buffer* (make-u8vector (recv-buff-size)))

; Unimplemented function codes

;(define force-multiple-coils 15)
;(define read-general-reference 20)
;(define write-general-reference 21)
;(define mask-write-register 22)
;(define read-write-register 23)
;(define read-fifo-queue 24)

; Prototypes

; For function codes 1, 2, 3, 4

(define (send-read-request! fc ui n offset port)
  (u8vector-set! *send-buffer* 5 6)
  (u8vector-set! *send-buffer* 6 ui)
  (u8vector-set! *send-buffer* 7 fc)
  (u8vector-set! *send-buffer* 9 offset)
;  (u8vector-set! *send-buffer* 8 (extract-bit-field 8 8 n))
;  (u8vector-set! *send-buffer* 9 (extract-bit-field 8 0 n))
  (u8vector-set! *send-buffer* 10 (extract-bit-field 8 8 n))
  (u8vector-set! *send-buffer* 11 (extract-bit-field 8 0 n))
  (write-subu8vector *send-buffer* 0 (u8vector-length *send-buffer*) port)
  (force-output port))

(define (recv-read-response! fc ui port)
  (read-subu8vector *recv-buffer* 0 6 port)
  (let* ((len (+ 6 (u8vector-ref *recv-buffer* 5))))
    (read-subu8vector *recv-buffer* 6 len port)
    (cond
      ((not (eqv? ui (u8vector-ref *recv-buffer* 6)))
       (raise "Mismatched Unit Id"))
      ((not (eqv? fc (u8vector-ref *recv-buffer* 7)))
       (raise "Function code mismatch"))
      ((not (eqv? (- len 9)  (u8vector-ref *recv-buffer* 8)))
       (raise "Wrong data length"))
      (else (subu8vector *recv-buffer* 9 len)))))

;;; For function codes 5, 6

(define (send-write-request! fc ui n offset port)
  (u8vector-set! *send-buffer* 5 6)
  (u8vector-set! *send-buffer* 6 ui)
  (u8vector-set! *send-buffer* 7 fc)
  (u8vector-set! *send-buffer* 9 offset)
  (u8vector-set! *send-buffer* 10 (extract-bit-field 8 8 n))
  (u8vector-set! *send-buffer* 11 (extract-bit-field 8 0 n))
  (write-subu8vector *send-buffer* 0 (u8vector-length *send-buffer*) port)
  (force-output port))

(define (recv-write-response! fc ui port)
  (read-subu8vector *recv-buffer* 0 6 port)
  (let* ((len (+ 6 (u8vector-ref *recv-buffer* 5))))
    (read-subu8vector *recv-buffer* 6 len port)
    (cond
      ((not (eqv? ui (u8vector-ref *recv-buffer* 6)))
       (raise "Mismatched Unit Id"))
      ((not (eqv? fc (u8vector-ref *recv-buffer* 7)))
       (raise "Function code mismatch"))
#;
      ((not (eqv? (- len 9)  (u8vector-ref *recv-buffer* 8)))
       (raise "Wrong data length"))
      (else (subu8vector *recv-buffer* 9 len)))))

; For function codes 7
; TODO: this should not be a prototype

(define (send-exception-status-request! fc ui port)
  (u8vector-set! *send-buffer* 5 2)
  (u8vector-set! *send-buffer* 6 ui)
  (u8vector-set! *send-buffer* 7 fc)
  (write-subu8vector *send-buffer* 0 (u8vector-length *send-buffer*) port)
  (force-output port))

(define (recv-exception-status-response! fc ui port)
  (read-subu8vector *recv-buffer* 0 6 port)
  (let* ((len (+ 6 (u8vector-ref *recv-buffer* 5))))
    (read-subu8vector *recv-buffer* 6 len port)
    (cond
      ((not (eqv? ui (u8vector-ref *recv-buffer* 6)))
       (raise "Mismatched Unit Id"))
      ((not (eqv? fc (u8vector-ref *recv-buffer* 7)))
       (raise "Function code mismatch"))
      ((not (eqv? (- len 9)  (u8vector-ref *recv-buffer* 8)))
       (raise "Wrong data length"))
      (else (subu8vector *recv-buffer* 9 len)))))

; High level interface


(define (read-coils! ui n offset #!optional (port (current-input-port)))
  (send-read-request! 1 ui n offset port)
  (recv-read-response! 1 ui port))


(define (read-input-discretes! ui n offset
                               #!optional (port (current-input-port)))
  (send-read-request! 2 ui n offset port)
  (recv-read-response! 2 ui port))


(define (read-multiple-registers! ui n offset
                                  #!optional (port (current-input-port)))
  (send-read-request! 3 ui n offset port)
  (recv-read-response! 3 ui port))


(define (read-input-registers! ui n offset
                               #!optional (port (current-input-port)))
  (send-read-request! 4 ui n offset port)
  (recv-read-response! 4 ui port))


(define (write-coil! ui n value #!optional (port (current-input-port)))
  (send-write-request! 5 ui (if value #xff00 #x0000) n port)
  (recv-write-response! 5 ui port))


(define (write-single-register! ui n value
                                #!optional (port (current-input-port)))
  (send-write-request! 6 ui value n port)
  (recv-write-response! 6 ui port))


(define (read-exception-status! ui #!optional (port (current-input-port)))
  (send-read-request! 7 ui port)
  (recv-read-response! 7 ui port))

;; Test

(define (cout . args) (for-each display args))

(define (test n offset)
  (let* ((s (open-tcp-client (list server-address: "localhost"
                                   port-number: 502)))
         (res (read-inputs! 1 n offset s)))
    (close-port s)
    (pp res)))

