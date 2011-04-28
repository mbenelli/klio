; logger.scm - Logging facilitis
;
; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.


(##namespace ("logger#"))
(##include "~~lib/gambit#.scm")
(##namespace ("datetime#" date->string current-date))
(##namespace ("strings#" string-pad string-upcase))


(define levels '((error . 0) (warning . 1) (info . 2) (debug . 3)))


(define current-logger-level
  (make-parameter 'info (lambda (x) (cdr (assq x levels)))))


(define (level-enabled? level)
  (<= (cdr (assq level levels)) (current-logger-level)))


(define (make-logger filename)
  (lambda (level msg)
    (if (level-enabled? level)
      (with-output-to-file `(path: ,filename append: #t)
	(lambda ()
	  (println 
            (date->string (current-date 0) "~a, ~d ~b ~Y ~T GMT")
	    #\space #\space
	    (string-pad (string-upcase (symbol->string level)) 7)
	    #\space #\space
	    msg))))))

