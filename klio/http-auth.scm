;; http-auth.scm - http authentication (rfc-2617)
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(##namespace ("http-auth#"))
(##include "~~lib/gambit#.scm")

(##namespace
  ("prelude#" and-let* push! split)
  ("lists#" assoc)
  ("irregex#" irregex irregex-search)
  ("base64#" u8vector->base64-string)
  ("http#" current-request request-attributes request-uri uri-path unauthorized))

(define *protected-paths* '())

(define add-realm-path
  (lambda (path realm)
    (push! (cons path realm) *protected-paths*)))

;; Return a pair (uri-path . realm)
(define current-realm
  (lambda ()
    (assoc (uri-path (request-uri (current-request)))
      *protected-paths*
      (lambda (key e)
        (if (irregex-search `(: bos ,e) key) #t #f)))))

(define *authorized* '())

(define add-user
  (lambda (name passwd realm)
    (push!
      (cons
        (u8vector->base64-string
          (with-output-to-u8vector
            '#u8()
            (lambda () (print name ":" passwd))))
        realm)
      *authorized*)))

(define check-auth
  (lambda (credentials)
    (let ((credentials ((split #\space) credentials)))
      (pp credentials)
      (cond
        ((string-ci=? "Basic" (car credentials))
         (and-let* ((user-realm (assoc (cadr credentials) *authorized*))
                    (path-realm (current-realm)))
           (eq? (cdr user-realm) (cdr path-realm))))
        ((string-ci=? "Digest" (car credentials))
         (pp "Digest authentication not supported.")
         #f)
        (else #f)))))


(define with-authentication
  (lambda (fn)
    (let* ((req (current-request)))
      (cond
        ((assoc "Authorization" (request-attributes req))
         => (lambda (x)
              (if (check-auth (cdr x))
                  (fn)
                  (unauthorized (cdr x)))))
        ((current-realm) => (lambda (x) (unauthorized (cdr x))))
        (else (fn))))))