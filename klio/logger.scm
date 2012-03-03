; logger.scm - Logging facilitis
;
; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.


(##namespace ("logger#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")
(##namespace
  ("rfc1123#" time->string)
  ("strings#" string-pad string-upcase))


(define levels '((error . 0) (warning . 1) (info . 2) (debug . 3)))


(define current-logger-level
  (make-parameter 'info (lambda (x) (cdr (assq x levels)))))


(define (level-enabled? level)
  (<= (cdr (assq level levels)) (current-logger-level)))


(define logger-mutex (make-mutex))

(define (make-logger filename)
  (lambda (level msg)
    (if (level-enabled? level)
      (with-mutex
       logger-mutex
       (with-output-to-file `(path: ,filename append: #t)
         (lambda ()
           (println
	    (time->string (current-time))
	    #\space #\space
	    (string-pad (string-upcase (symbol->string level)) 7)
	    #\space #\space
	    msg)))))))

