;; kliows.scm - Klio web server launcher
;;
;; Copyright (c) 2010, 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(include "prelude.scm")
(include "lists.scm")
(include "charsets.scm")
(include "strings.scm")
(include "input-parse.scm")
(include "ssax.scm")
(include "sxml.scm")
(include "base64.scm")
(include "datetime.scm")
(include "http.scm")
(include "kws.scm")

(define (main args)
  (case (length args)
    ((1) (kws#kws port-number: (string->number (car args)) multithread: #t))
    ((2) (kws#kws port-number: (string->number (car args))
           server-root: (cadr args) multithread: #t))
    (else (kws#kws multithread: #t))))

(main (cdr (command-line)))
