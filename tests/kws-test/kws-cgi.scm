;; kws-cgi.scm - Klio web server cgi test.
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

;(include "../../klio/prelude.scm")
;(include "../../klio/http.scm")
;(include "../../klio/kws.scm")
;(include "../../klio/cgi.scm")

;(include "~~lib/gambit#.scm")

(##namespace ("http#" current-request request-uri uri-path reply))
(##namespace ("kws#" get-static kws *server-root*))
(##namespace ("cgi#" cgi-path run-cgi))

(define (cgi? p)
  (member p '("/test-cgi")))

(define (dispatch)
  (let ((p (uri-path (request-uri (current-request)))))
    (cond
      ((cgi? p) (parameterize ((cgi-path (*server-root*)))
                  (pp (run-cgi p))
		  (reply (lambda () (print (run-cgi p))))))
      (else (raise "Not a cgi")))))

(kws
  port-number: 8000
  multithread: #f
  dispatcher: dispatch
  ;server-root: "."
  )

