;; kws.scm - Klio web server
;;
;; Copyright (c) 2010 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(##namespace ("kws#"))
(##include "~~lib/gambit#.scm")
(##include "base64#.scm")
(##include "http#.scm")
(##include "prelude#.scm")
(##namespace ("datetime#" date->string make-time time-utc->date))
(##namespace ("charsets#" char-set char-set-complement))
(##namespace ("strings#" string-contains-ci string-tokenize))
(##namespace ("sxml#" srl:parameterizable))

(declare
  (standard-bindings)
  (extended-bindings)
  (block))


(define (send-response thunk)
  (let ((response (with-exception-catcher
                    (lambda (e)
                      `(html (head (title "Exception"))
                         (body (h2 "Internal server error")
                           (pre ,(call-with-output-string ""
                                   (lambda (p) (##display-exception e p)))))))
                    thunk)))
    (reply
      (lambda ()
        (srl:parameterizable
          response
          (current-output-port)
          '(indent . #t)
          '(method . html))))))


(define (unknown-page)
  `(html (head (title "Unknown page"))
	 (body (p "The requested page is unknown"))))


;;; Pages


(define *pages* (make-table test: string=?))


(table-set! *pages* "/rcgi.bin/jvmForm"
  (lambda (args)
    `(result (status "KO") (data "Not yet implemented"))))


(table-set! *pages* "/test"
  (lambda (args)
    `(html (head (title "Test"))
       (body (h1 "Test") (p "Test")))))


(table-set! *pages* "/inspect"
  (lambda (args)
    `(html (head (title "Data inspector"))
       (body
         (h2 "Data")
         ,(if args
              `(ul ,@(map
                       (lambda (x) `(li ,(car x) ": " ,(cdr x)))
                       args))
              `(a "There are no data."))
         (a (@ (href "/index.html")) "Back")))))

(define run-query
  (lambda (q nrows)
    (or
      (and-let* ((start (string-contains-ci q "SELECT"))
		 (s (+ 7 start))
                 (e (string-contains-ci q "FROM"))
                 (fields (string-tokenize q
                           (char-set-complement (char-set #\space #\,))  s e))
                 (nfields (length fields)))
        (with-output-to-string q
          (lambda ()
            (print #\newline #\newline #\newline)
            (do ((i 0 (+ 1 i)))
               ((= i nrows) #!void)
              (print "r[" i "]=")
              (do ((j 0 (+ 1 j)))
                  ((= j nfields) (newline))
                (print (random-integer 2) #\tab))))))
      "Invalid Query")))

(table-set! *pages* "/dataRecover.cgi"
  (lambda (args)
    (or
      (and-let* ((data (assoc "query" args)) (query (cdr data)))
        (run-query query 10))
      "Error")))


(table-set! *pages* "/shutdown"
  (lambda (args)
    (exit)))

(define *server-root* (make-parameter (current-directory)))


(define (static-content path)
  (call-with-input-file
      (list
        path: (string-append
                (path-strip-trailing-directory-separator (*server-root*))
                path))
    (lambda (in)
      (let loop ((b (read-u8 in)))
        (cond
          ((eof-object? b) (write-u8 b))
          (else
            (write-u8 b)
            (loop (read-u8 in))))))))


(define (static-content-old path)
  (with-exception-catcher
    (lambda (e)
      (if (no-such-file-or-directory-exception? e)
          (not-found)))
    (lambda ()
      (call-with-input-file
          (list
            path: (string-append
                    (path-strip-trailing-directory-separator (*server-root*))
                    path))
        (lambda (in)
          (let loop ((b (read-u8 in)))
            (cond
              ((eof-object? b) (write-u8 b))
              (else
                (write-u8 b)
                (loop (read-u8 in)))))
          )))))


;;;

(define (mime path)
  (or
    (and-let* ((mimetype (assoc (path-extension path)
                           '((".html" . "text/html")
                             (".htm"  . "text/html")
                             (".css"  . "text/css")
                             (".txt"  . "text/plain")
                             (""      . "text/plain")
                             (".js"   . "application/javascript")
                             (".json" . "application/json")
                             (".bmp"  . "image/bmp")
                             (".ico"  . "image/ico")
                             (".jpg"  . "image/jpg")
                             (".jpeg" . "image/jpeg")
                             (".png"  . "image/png")))))
      (cdr mimetype))
    "text/plain"))

(define (last-modified path)
  (let* ((filename (string-append
                     (path-strip-trailing-directory-separator (*server-root*))
                     path))
         (utc (file-info-last-modification-time (file-info filename))))
    (pp utc)
    (pp (inexact->exact (truncate (time->seconds utc))))
    (date->string
      (time-utc->date
        (make-time 'time-utc 0
          (inexact->exact (truncate (time->seconds utc))))))))

(define (dispatch)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (pp (request-connection request))
    (force-output (current-error-port))
    (or
      (and-let* ((generator (table-ref *pages* path #f)))
        (send-response (lambda () (generator (uri-query uri)))))
      (reply (lambda () (static-content-old path))
        mime: (mime path)
        ;; last-modified: (last-modified path)
        ))))


(define (get)
  (send-response
    (lambda ()
      (let* ((request (current-request))
	     (uri (request-uri request))
	     (path (uri-path uri))
	     (generator (table-ref *pages* path unknown-page)))
	(generator)))))


(define (post)
  (send-response
    (lambda ()
      (let* ((request (current-request))
	     (uri (request-uri request))
	     (path (uri-path uri))
	     (generator (table-ref *pages* path unknown-page)))
	(generator (request-query request))))))


(define (kws #!key
          (port-number 8000)
          (multithread #f)
          (server-root (*server-root*)))

  (println port: (current-error-port)
    "\nStarting Klio Web Server\n\n"
    "Port: " port-number #\newline
    "Server root: " server-root #\newline
    "Multithreading: " (if multithread "enabled" "disabled") #\newline)
  (force-output (current-error-port))

  (parameterize ((*server-root* server-root))
    (pp (*server-root*))
    (force-output (current-error-port))
    (http-server-start!
      (make-http-server
        port-number: port-number
        threaded?: multithread
        GET: dispatch
        POST: post))))
