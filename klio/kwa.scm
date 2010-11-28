(##namespace ("kwa#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")
(##namespace ("http#" reply current-request request-connection
              request-uri uri-path uri-query))
(##namespace ("kws#" kws get-static))
(##namespace ("charsets#" char-set char-set-complement))
(##namespace ("strings#" string-contains-ci string-tokenize))
(##namespace ("sxml#" srl:parameterizable))

;; Andrew Whaley's mysql library needed.  See dumping grouds.
;;(##namespace ("mysql#" sql-connect sql-execute sql-close-connection))


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

(define run-query-fake
  (lambda (q #!optional (nrows 10))
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

#;(define run-query
  (lambda (q)
    (let* ((db (sql-connect "localhost" 3306 "db" "user" "passwd"))
           (res (sql-execute q db))
           (response (with-output-to-string
                       q
                       (lambda ()
                         (print #\newline #\newline)
                         (println (car res))
                         (for-each
                           (lambda (record)
                             (print "r[i]=")
                             (for-each
                               (lambda (field)
                                 (print field #\tab))
                               record)
                             (newline))
                           (cdr res))))))
      (sql-close-connection db)
      response)))

(table-set! *pages* "/dataRecover.cgi"
  (lambda (args)
    (or
      (and-let* ((data (assoc "query" args)) (query (cdr data)))
        (run-query-fake query))
      "Error")))


(table-set! *pages* "/shutdown"
  (lambda (args)
    (exit)))


(define (dispatch)
  (let* ((request (current-request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (pp (request-connection request))
    (force-output (current-error-port))
    (or
      (and-let* ((generator (table-ref *pages* path #f)))
        (send-response (lambda () (generator (uri-query uri)))))
      (get-static path))))


(kws
  port-number: 8000
  multithread: #t
  server-root: "~/Desktop/hera"
  dispatcher: dispatch)