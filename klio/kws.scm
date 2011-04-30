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

(declare
  (standard-bindings)
  (extended-bindings)
  (block))


(define *server-root* (make-parameter (current-directory)))


(define (static-content path)
  (let* ((p (open-input-file path))
         (f (read-line p #f)))
    (close-input-port p)
    (display f)))

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
  (let ((utc (file-info-last-modification-time (file-info path))))
    (date->string
      (time-utc->date
        (make-time 'time-utc 0
          (inexact->exact (truncate (time->seconds utc)))))
      "~a, ~d ~b ~Y ~T GMT")))


#;
(define get-static
  (lambda (#!optional (path (uri-path (request-uri (current-request)))))
    (reply (lambda () (static-content path))
      mime: (mime path)
                                        ;last-modified: (last-modified path)
      )))

(define get-static
  (lambda ()
    (with-exception-catcher
      (lambda (e)
        (if (no-such-file-or-directory-exception? e)
            (not-found)))
      (lambda ()
        (let ((path (string-append
                      (path-strip-trailing-directory-separator (*server-root*))
                      (uri-path (request-uri (current-request))))))
          (reply (lambda () (static-content path))
	    `(("Content-Type" . ,(mime path))
	      ("Last-Modified" . ,(last-modified path)))
            ))))))


(define (kws #!key
          (port-number 8000)
          (multithread #f)
          (dispatcher #f)
          (server-root (*server-root*)))

  (println port: (current-error-port)
    "\nStarting Klio Web Server\n\n"
    "Port: " port-number #\newline
    "Server root: " server-root #\newline
    "Multithreading: " (if multithread "enabled" "disabled") #\newline)
  (force-output (current-error-port))

  (parameterize ((*server-root* server-root))
                     (http-server-start!
                       (make-http-server
                         port-number: port-number
                         threaded?: multithread
                         GET: (or dispatcher get-static)
                         POST: (or dispatcher get-static)))))

