; test.scm - Poor man's test suite.
;
; Copyright (c) 2011 Marco Benelli <mbenelli@yahoo.com>
; All right reserved.
;

(##namespace ("test#"))
(##include "~~lib/gambit#.scm")

(define (display* . args) (for-each display args))

(define-macro (test exp res)
  `(let* ((v ,exp)
          (r ,res)
          (explen (string-length
                    (with-output-to-string ""
                      (lambda () (write ',exp)))))
          (reslen (string-length
                    (with-output-to-string ""
                      (lambda () (write ,res)))))
          (fill (make-string (- 68 explen reslen) #\space)))
    (if (equal? v r)
        (display* ',exp " => " r fill "succeded" #\newline)
        (display* ',exp " => " r fill "failed" #\newline))))

