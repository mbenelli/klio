;; prelude.scm - A prelude for Gambit-C scheme.
;;
;; Copyright (c) 2008-2010 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.


(##namespace ("prelude#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe))

;; Functionals

(define identity values)

(define (const v) (lambda () v))

(define (compose . fs)
  (if (null? fs)
      identity
      (let ((f (car fs))
	    (g (apply compose (cdr fs))))
	(lambda (x)
	  (f (g x))))))

;;; String

(define (str x)
  (with-output-to-string "" (lambda () (write x))))


;;; Output

(define (cout . args)
  (for-each (lambda (x) (if (procedure? x) (x) (display x))) args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
		  (x (current-error-port))
		  (display x (current-error-port))))
            args))

(define nl (string #\newline))

;;; char-encodings

(define ascii->char integer->char)
(define ucscode->char integer->char)
(define char-return (ascii->char 13))
(define char-tab    (ascii->char 9))
(define char-newline (ascii->char 10)) ; a.k.a. #\newline, per R5RS

;;; parser-error-vanilla

(define (parser-error port message . rest)
  (apply error message rest))

;;; Binary integer representation

(define (bin n)
  (let loop ((n n) (bin '()))
    (let ((q (quotient n 2))
          (r (remainder n 2)))
      (if (= n 0)
          bin
          (loop q (cons r bin))))))

(define (dec bin)
  (let loop ((bin (reverse bin)) (e 0) (res 0))
    (if (null? bin)
        res
        (loop (cdr bin) (+ 1 e) (+ res (* (car bin) (expt 2 e)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gambit specific

;;; strings

(define (split separator)
  (lambda (str)
    (call-with-input-string
      str
      (lambda (s)
        (read-all s (lambda (s) (read-line s separator)))))))


;;; Memoize functions calls

(define (memoize f)
  (let ((cache (make-table))
	(not-found (gensym)))
    (lambda k
      (let ((v (table-ref cache k not-found)))
	(if (eq? v not-found)
	    (let ((res (apply f k)))
	      (table-set! cache k res)
	      res)
	    v)))))


