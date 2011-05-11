;; cgi.scm - cgi support for Klio web server
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

;; FIXME: hondle http headers

(##namespace ("cgi#"))
(##include "~~lib/gambit#.scm")
(##namespace ("http#" current-request request-query request-uri uri-query))
(##namespace ("kws#" *server-root*))

(define cgi-path
  (make-parameter
    (string-append
      (path-strip-trailing-directory-separator (*server-root*))
      "/cgi-bin/")))

(define (run-cgi name)
  (let* ((req (current-request))
	 (env (append (or (request-query req) '())
		      (or (uri-query (request-uri req)) '()))))
    (call-with-input-process
      `(path: ,(path-expand (path-strip-directory name) (cgi-path))
	environment: ,(map (lambda (x) (with-output-to-string ""
					 (lambda ()
					   (print (car x) #\= (cdr x)))))
			   env))
      (lambda (p) (read-line p #f)))))

