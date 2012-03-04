; kws-db.scm - sqlite test for klio web server
;
; Copyright (c) 2008-2010 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.

(##namespace ("kws-db#"))
(##include "~~lib/gambit#.scm")
(##include "../klio/prelude#.scm")
(##namespace
  ("uri#" uri-query uri-path)
  ("http-srv#" reply current-request request-uri reply-chunked)
  ("sqlite3#" sqlite3)
  ("kws#" get-static *server-root*))

;;; Database handling

(define dbname (make-parameter "test.db"))

(define-type dbhandler
  constructor: %make-dbhandler
  run
  close)

(define (make-dbhandler dbpath)
  (call-with-values
    (lambda () (sqlite3 dbpath))
    (lambda (hdl close)
      (%make-dbhandler hdl close))))

(define (display-row seed . cols)
  (println (map (lambda (x) (cons x #\space)) cols))
  (values #t ""))


;;; Pages

(define pages (make-table test: string=?))

(table-set! pages "/select"
  (lambda ()
    (and-let* ((q (uri-query (request-uri (current-request))))
	       (t (assoc "table" q))
	       (table (cdr t)))
      (let ((db (make-dbhandler (string-append (*server-root*) (dbname)))))
        ((dbhandler-run db) display-row ""
         (string-append "SELECT * FROM " table))
        ((dbhandler-close db))))))

(table-set! pages "/chunks"
  (lambda (send-chunks)
    (let ((send-row (lambda (seed . cols)
                      (send-chunks
                        (lambda ()
                          (println (map (lambda (x) (cons x #\space)) cols))))
                      (values #t ""))))
      (and-let* ((q (uri-query (request-uri (current-request))))
                 (t (assoc "table" q))
                 (table (cdr t)))
        (let ((db (make-dbhandler (string-append (*server-root*) (dbname)))))
          ((dbhandler-run db) send-row "" (string-append "SELECT * FROM " table))
          ((dbhandler-close db)))))))


(define (dispatcher)
  (let ((path (uri-path (request-uri (current-request)))))
    (cond
      
      ((string=? path "/chunks")
       (reply-chunked (table-ref pages "/chunks")))
      
      ((table-ref pages path #f)
       => (lambda (x)
            (reply x)))
      (else (get-static)))))

