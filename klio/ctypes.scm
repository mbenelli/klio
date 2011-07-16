;;; floats.scm - Reading and writing C types from byte ports.
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

(##namespace ("ctypes#"))
(##include "~~lib/gambit#.scm")

(define u8vector-subtype (##subtype (u8vector)))
(define s8vector-subtype (##subtype (s8vector)))
(define u16vector-subtype (##subtype (u16vector)))
(define s16vector-subtype (##subtype (s16vector)))
(define u32vector-subtype (##subtype (u32vector)))
(define s32vector-subtype (##subtype (s32vector)))
(define u64vector-subtype (##subtype (u64vector)))
(define s64vector-subtype (##subtype (s64vector)))
(define f32vector-subtype (##subtype (f32vector)))
(define f64vector-subtype (##subtype (f64vector)))


(define-macro (define-writer name vtype)
  `(define (,name x #!optional (port (current-output-port)))
     (let ((v (,vtype x)))
       (##subtype-set! v u8vector-subtype)
       (write-subu8vector v 0 (u8vector-length v) port))))

(define-macro (define-reader name vtype vsubtype vtype-ref init)
  `(define (,name #!optional (port (current-input-port)))
     (let ((v (,vtype ,init)))
       (##subtype-set! v u8vector-subtype)
       (let ((n (read-subu8vector v 0 (u8vector-length v) port)))
         (if (= n (u8vector-length v))
             (begin
               (##subtype-set! v ,vsubtype)
               (,vtype-ref v 0))
             #!eof)))))

(define-writer write-s8 s8vector)
(define-reader read-s8 s8vector s8vector-subtype s8vector-ref 0)

(define-writer write-u16 u16vector)
(define-reader read-u16 u16vector u16vector-subtype u16vector-ref 0)

(define-writer write-s16 s16vector)
(define-reader read-s16 s16vector s16vector-subtype s16vector-ref 0)

(define-writer write-u32 u32vector)
(define-reader read-u32 u32vector u32vector-subtype u32vector-ref 0)

(define-writer write-s32 s32vector)
(define-reader read-s32 s32vector s32vector-subtype s32vector-ref 0)

(define-writer write-u64 u64vector)
(define-reader read-u64 u64vector u64vector-subtype u64vector-ref 0)

(define-writer write-s64 s64vector)
(define-reader read-s64 s64vector s64vector-subtype s64vector-ref 0)

(define-writer write-f32 f32vector)
(define-reader read-f32 f32vector f32vector-subtype f32vector-ref 0.0)

(define-writer write-f64 f64vector)
(define-reader read-f64 f64vector f64vector-subtype f64vector-ref 0.0)
