;; simple.scm - Minimal service
;;
;; Copyright (c) 2011-2012 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(##namespace ("simple#"))
(##include "~~lib/gambit#.scm")
(##include "../../klio/prelude#.scm")

(##namespace
  ("uri#" uri-path uri-query)
  ("http-srv#" reply current-request request-query request-uri)
  ("kws#" kws get-static get-file *server-root*)
  ("sxml#" srl:sxml->html))

(define (post-value name)
  (pp (current-request))
  (and-let* ((query (request-query (current-request)))
             (kv-pair (assoc name query)))
    (cdr kv-pair)))

(define (get-value name)
  (and-let* ((query (uri-query (request-uri (current-request))))
             (kv-pair (assoc name query)))
    (cdr kv-pair)))


                                        ; Pages

(define pages (make-table test: string=?))

(table-set! pages "/form"
  (lambda ()
    (srl:sxml->html
      `(html
         (head (title "Form example"))
         (body
           (form (@ (action "response") (method "POST"))
             (a "Enter your name:")
             (input (@ (type "text") (name "data")))
             (input (@ (type "submit") (value "Ok"))))))
      (current-output-port))))


(table-set! pages "/response"
  (lambda ()
    (srl:sxml->html
      `(html
         (head (title "Response"))
         (body (p "Your name is:" ,(post-value "data"))))
      (current-output-port))))


(define (dispatch)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (cond
      ((string=? path "/") (get-file "/index.html" '()))
      ((table-ref pages path #f) => (lambda (x) (reply x)))
      (else (get-static)))))
