;;; floats.scm - Reading and writing floting point values
;;;
;;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;;; All Right Reserved.
;;;
;;; Author: Marco Benelli <mbenelli@yahoo.com>
;;;
;;;
;;; Based on suggestions from Marc Feeley on Gambit's mailing list:
;;; https://mercure.iro.umontreal.ca/pipermail/gambit-list/2009-June/003671.html
;;;
;;; Notes:
;;;
;;; IEEE 754 representation is no guaranteed.
;;; Endianess is determined by the processor's native endianess

(##namespace ("floats#"))
(##include "~~lib/gambit#.scm")

(define u8vector-subtype (##subtype (u8vector)))
(define f32vector-subtype (##subtype (f32vector)))
(define f64vector-subtype (##subtype (f64vector)))


(define (write-f32 x port)
  (let ((v (f32vector x)))
    (##subtype-set! v u8vector-subtype)
    (write-subu8vector v 0 (u8vector-length v) port)))

(define (read-f32 port)
  (let ((v (f32vector 0.0)))
    (##subtype-set! v u8vector-subtype)
    (let ((n (read-subu8vector v 0 (u8vector-length v) port)))
      (if (= n (u8vector-length v))
        (begin
          (##subtype-set! v f32vector-subtype)
          (f32vector-ref v 0))
        #!eof))))

(define (write-f64 x port)
  (let ((v (f64vector x)))
    (##subtype-set! v u8vector-subtype)
    (write-subu8vector v 0 (u8vector-length v) port)))

(define (read-f64 port)
  (let ((v (f64vector 0.0)))
    (##subtype-set! v u8vector-subtype)
    (let ((n (read-subu8vector v 0 (u8vector-length v) port)))
      (if (= n (u8vector-length v))
        (begin
          (##subtype-set! v f64vector-subtype)
          (f64vector-ref v 0))
        #!eof))))


