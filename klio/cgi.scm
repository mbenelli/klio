; cgi.scm - cgi support for Klio web server
;
; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.

; TODO
; - set environment dinamically based or real values
; - handle POST requests: read request-query and pass to stdin of called
;   program

(##namespace ("cgi#"))
(##include "~~lib/gambit#.scm")
(##namespace ("http#" current-request request-query request-uri uri-query
               request-connection request-version keep-alive? response-date
               serve-connection request-server encode-x-www-form-urlencoded))
(##namespace ("kws#" *server-root*))

(define cgi-path
  (make-parameter
    (string-append
      (path-strip-trailing-directory-separator (*server-root*))
      "/cgi-bin/")))

(define (run-cgi name)
  (let* ((req (current-request))
	 (env (or (uri-query (request-uri req)) '()))
	(qs (string-append "QUERY_STRING="
	      (encode-x-www-form-urlencoded (uri-query (request-uri req))))))
    (call-with-input-process
      `(path: ,(path-expand (path-strip-directory name) (cgi-path))
	environment: ,(list qs
	                "SERVER_SOFTWARE=Klio Web Server"
	                "SERVER_NAME=192.168.1.47"
			"GATEWAY_INTERFACE=CGI/1.1"
			"SERVER_PROTOCOL=HTTP/1.1"
			"SERVER_PORT=8000"
			"REQUEST_METHOD=GET"))
      (lambda (p) (read-line p #f)))))

(define (cgi-reply thunk)
  (let* ((request (current-request))
	 (connection (request-connection request))
	 (version (request-version request))
	 (to-be-closed (not (keep-alive? request)))
	 (eol "\r\n"))
    (with-output-to-port connection
      (lambda ()
	(print
	  version " 200 OK" eol
          "Server: Klio Web Server" eol
          "Date: " (response-date) eol
          (if to-be-closed "Connection: close\r\n" ""))
        (thunk)))
    (cond
      (to-be-closed (close-port connection))
      (else (force-output connection)
            (serve-connection (request-server request) connection)))))

