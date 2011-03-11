;; http-auth.scm - http authentication (rfc-2617)
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(##namespace ("http-auth#"))
(##include "~~lib/gambit#.scm")
(##namespace ("irregex#" irregex irregex-search))

(define *protected-paths*
  (list->table
    '(("/private/" . realm1)
      ("/subdir/private/" . realm2))))

(define need-auth?
  (lambda (path)
    (if (table-search (lambda (k v) (irregex-search `(: bos ,k) path))
          *protected-paths*)
        #t
        #f)))


(define check-auth
  (lambda (credentials)
    ;; TODO
    (unauthorized)))


(define with-authentication
  (lambda (fn)
    (let* ((req (current-request)))
      (cond
        ((assoc "WWW-Authenticate" (request-attributse req)) => check-auth)
        ((need-auth? (uri-path (request-uri req))) (unauthorized))
        (else (fn))))))