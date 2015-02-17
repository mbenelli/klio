;; Copyright (c) 2010, 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.
;;
;; Original copyright notice:

; Copyright (c) 2005-2008 by Marc Feeley, All Rights Reserved.

;==============================================================================

(##namespace ("http-srv#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")
(##namespace
  ("strings#" string-contains-ci)
  ("uri#" parse-uri uri-query decode-x-www-form-urlencoded)
  ("rfc1123#" time->string))

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe))

;; Parameters

(define max-connections (make-parameter 200))


;==============================================================================

; Token tables.

(define hash-substring
  (lambda (str start end)

    (define loop
      (lambda (h i)
        (if (< i end)
            (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                          65536)
                  (+ i 1))
            h)))

    (loop 0 start)))

(define-macro make-token-table
  (lambda alist

    ; "alist" is a list of lists of the form "(string expression)"

    ; The result is a perfect hash-table represented as a vector of
    ; length 2*N, where N is the hash modulus.  If the string S is in
    ; the hash-table it is at index
    ;
    ;   X = (* 2 (modulo (hash-substring S 0 (string-length S)) N))
    ;
    ; and the associated expression is at index X+1.

    (define hash-substring    ; repeated from above to be
      (lambda (str start end) ; available for macro expansion

        (define loop
          (lambda (h i)
            (if (< i end)
                (loop (modulo (+ (* h 5063) (char->integer (string-ref str i)))
                              65536)
                      (+ i 1))
                h)))

        (loop 0 start)))

    (define make-perfect-hash-table
      (lambda (alist)
        (let loop1 ((n (length alist)))
          (let ((v (make-vector (* 2 n) #f)))
            (let loop2 ((lst alist))
              (if (pair? lst)
                  (let* ((x (car lst))
                         (str (car x)))
                    (let ((h
                           (* 2
                              (modulo (hash-substring str 0 (string-length str))
                                      n))))
                      (if (vector-ref v h)
                          (loop1 (+ n 1))
                          (begin
                            (vector-set! v h str)
                            (vector-set! v (+ h 1) (cadr x))
                            (loop2 (cdr lst))))))
                  v))))))

      (cons 'vector (vector->list (make-perfect-hash-table alist)))))

(define token-table-lookup-substring
  (lambda (table str start end)
    (let* ((n (quotient (vector-length table) 2))
           (h (* 2 (modulo (hash-substring str start end) n)))
           (x (vector-ref table h)))

      (define loop
        (lambda (i j)
          (if (< i end)
              (if (char=? (string-ref str i) (string-ref x j))
                  (loop (+ i 1) (+ j 1))
                  #f)
              h)))

      (and x
           (= (string-length x) (- end start))
           (loop start 0)))))

(define token-table-lookup-string
  (lambda (table str)
    (token-table-lookup-substring table str 0 (string-length str))))

;==============================================================================

; HTTP server.

(define-type server
  id: c69165bd-c13f-11d9-830f-00039301ba52

  port-number
  timeout
  threaded?
  method-table
)

(define-type request
  id: 8e66862f-c143-11d9-9f4e-00039301ba52

  (server unprintable:)
  connection
  method
  uri
  version
  attributes
  query
)

;; Persistent connection handling
(define keep-alive?
  (lambda (request)
    (cond
      ((not (server-threaded? (request-server request))) #f)
      ((assoc "Connection" (request-attributes request))
       => (lambda (x) (string-contains-ci (cdr x) "keep-alive")))
      (else #f))))

(define make-http-server
  (##lambda (#!key
           (port-number 80)
           (timeout     300)
           (threaded?   #f)
           (OPTIONS     unimplemented-method)
           (GET         unimplemented-method)
           (HEAD        unimplemented-method)
           (POST        unimplemented-method)
           (PUT         unimplemented-method)
           (DELETE      unimplemented-method)
           (TRACE       unimplemented-method)
           (CONNECT     unimplemented-method))
    (make-server
     port-number
     timeout
     threaded?
     (make-token-table
      ("OPTIONS" OPTIONS)
      ("GET"     GET)
      ("HEAD"    HEAD)
      ("POST"    POST)
      ("PUT"     PUT)
      ("DELETE"  DELETE)
      ("TRACE"   TRACE)
      ("CONNECT" CONNECT)))))

(define http-server-start!
  (lambda (hs)
    (let ((server-port
           (open-tcp-server
            (list server-address: '#u8(0 0 0 0)
                  port-number: (server-port-number hs)
                  backlog: 128
                  reuse-address: #t
                  char-encoding: 'ISO-8859-1))))
      (accept-connections hs server-port))))

(define accept-connections
  (lambda (hs server-port)
    (let loop ()
      (let ((connection (read server-port))
            (n-active (length (thread-group->thread-list
                                (thread-thread-group (current-thread))))))
        (if (server-threaded? hs)
            (if (< n-active (max-connections))
                (let ((dummy-port (open-dummy)))
                  (parameterize ((current-input-port dummy-port)
                                 (current-output-port dummy-port))
                    (thread-start!
                      (make-thread
                        (lambda ()
                          (serve-connection hs connection))))))
                (service-unavailable-error connection))
            (serve-connection hs connection)))
      (loop))))


(define (send-error version connection text)
  (print port: connection
    (or
      version
      (and-let* ((request (current-request))
                 (version (request-version request)))
        version)
      "HTTP/1.1")
    #\space text "\r\n\r\n")
  (close-port connection))


(define (method-not-implemented-error connection #!optional (version #f))
  (send-error version connection "501 Method not implemented"))


(define unimplemented-method
  (lambda ()
    (let* ((request (current-request))
           (version (request-version request))
           (connection (request-connection request)))
      (method-not-implemented-error connection version))))


(define (service-unavailable-error connection)
  (send-error #f connection "503 Service Unavailable"))


(define (bad-request-error connection #!optional (version #f))
  (send-error version connection "400 Bad Request"))


(define (reply-with-status-code text #!optional (headers '()))
  (let* ((request (current-request))
         (connection (request-connection request))
         (keep (keep-alive? request))
         (eol "\r\n"))
    (print port: connection
      (list
        (request-version request) " " text eol
        (with-output-to-string
          ""
          (lambda ()
            (for-each (lambda (x)
                        (print (car x) ": " (cdr x) eol))
              headers)))
        (if keep
          "Content-Length: 0\r\n\r\n"
          "Connection: Close\r\n\r\n")))
    (cond
      ((keep-alive? request)
       (force-output connection)
       (serve-connection (request-server request) connection))
      (else (close-port connection)))))


(define see-other
  (lambda (attrs)
    (reply-with-status-code "303 See Other" attrs)))

(define not-found
  (lambda () (reply-with-status-code "404 Not Found")))

(define unauthorized
  (lambda (realm)
    (reply-with-status-code "401 Unauthorized"
      `(("WWW-Authenticate" . ,(with-output-to-string
                                 "Basic realm="
                                 (lambda ()
                                   (print #\" realm #\"))))))))

(define internal-error
  (lambda () (reply-with-status-code "500 Internal Error")))

(define not-implemented
  (lambda () (reply-with-status-code "501 Not Implemented")))

(define service-unavaible
  (lambda () (reply-with-status-code "503 Service Unavaible")))

(define (response-date)
  (time->string (current-time)))

(define (reply-unbuffered thunk #!optional (attributes '()))
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
        (for-each
          (lambda (x) (print (car x) ": " (cdr x) eol))
          attributes)
        (print eol)
        (thunk)))
    (cond
      (to-be-closed (close-port connection))
      (else (force-output connection)
        (serve-connection (request-server request) connection)))))

(define reply
  (lambda (thunk #!optional (attributes '())
            #!key
            (mime "text/html; charset=ISO-8859-1")
            (last-modified #f))
    (let* ((request
            (current-request))
           (connection
            (request-connection request))
           (version
            (request-version request))
           (to-be-closed (not (keep-alive? request))))

      (define generate-reply
        (lambda (port)
          (if (or (eq? version 'HTTP/1.0)
                  (eq? version 'HTTP/1.1))
              (let ((message (with-output-to-u8vector '() thunk))
                    (eol "\r\n"))
                (print port: port
                  (list version " 200 OK" eol
		    "Server: Klio Web Server" eol
                    "Content-Length: " (u8vector-length message) eol
                    "Date: " (response-date) eol
                    (if to-be-closed
                        (string-append "Connection: close" eol)
                        "")))
		(for-each
		  (lambda (x) (print port: port (car x) ": " (cdr x) eol))
		  attributes)
		(print port: port eol)
                (if (not (string=? (request-method request) "HEAD"))
                    (write-subu8vector
                      message
                      0
                      (u8vector-length message)
                      port))))))

      (generate-reply connection)

      (cond
        (to-be-closed
          (close-port connection))
        (else
          (force-output connection)
          (serve-connection (request-server request) connection))))))


;; Chunked reply
;;
;; Building blocks.
;; These function are not local to REPLY-CHUNKED for testing and
;; experimenting purposes.

(define send-chunked-reply-header
  (lambda (#!optional (attributes '()))
    (let* ((request (current-request))
           (connection (request-connection request))
           (version (request-version request))
           (to-be-closed (not (keep-alive? request)))
           (eol "\r\n"))
      (print port: connection
        (list
          version " 200 OK" eol
          "Server: Klio Web Server" eol
          "Date: " (response-date) eol
          "Transfer-Encoding: chunked" eol
          (if to-be-closed
              (string-append "Connection: close" eol)
              "")))
      (for-each
        (lambda (x) (print port: connection (car x) ": " (cdr x) eol))
        attributes)
      (print port: connection eol))))


(define send-chunk
  (lambda (thunk)
    (let* ((request (current-request))
           (connection (request-connection request)))
      (let* ((message (with-output-to-u8vector '#u8() thunk))
             (len (u8vector-length message)))
        (print port: connection (number->string len 16) "\r\n")
        (write-subu8vector message 0 len connection)
        (print port: connection "\r\n")))))


(define send-last-chunk
  (lambda ()
    (let* ((request (current-request))
           (connection (request-connection request)))
      (print port: connection "0\r\n\r\n")
      (cond
        ((not (keep-alive? request))
         (close-port connection))
        (else (force-output connection)
              (serve-connection (request-server request) connection))))))

;; Interface.

(define reply-chunked
  (lambda (thunk #!optional (attributes '()))
    (send-chunked-reply-header)
    (thunk send-chunk)
    (send-last-chunk)))


(define current-request
  (lambda ()
    (thread-specific (current-thread)))) ; request is stored in thread

;------------------------------------------------------------------------------

(define serve-connection
  (lambda (hs connection)

    ; Configure the connection with the client so that if we can't
    ; read the request after 300 seconds, the read operation will fail
    ; (and the thread will terminate).

    (input-port-timeout-set! connection 300)

    ; Configure the connection with the client so that if we can't
    ; write the response after 300 seconds, the write operation will
    ; fail (and the thread will terminate).

    (output-port-timeout-set! connection 300)

    (let ((req (permissive-read-line connection)))
      (if (not (string? req))
          (bad-request-error connection)
          (let* ((end
                  (let loop ((i 0))
                    (cond ((= i (string-length req))
                           #f)
                          ((char=? (string-ref req i) #\space)
                           i)
                          (else
                           (loop (+ i 1))))))
                 (method-index
                  (and end
                       (token-table-lookup-substring
                        (server-method-table hs)
                        req
                        0
                        end))))
            (if method-index

                (parse-uri
                 req
                 (+ end 1)
                 (string-length req)
                 #t
                 (lambda (uri i)

                   (define handle-version
                     (lambda (version)
                       (case version
                         ((HTTP/1.0)
                          (let ((attributes (read-header connection)))
                            (if attributes
                                (handle-request version attributes)
                                (bad-request-error connection version))))
                         ((HTTP/1.1)
                          (let ((attributes (read-header connection)))
                            (cond
                              (attributes (if (assoc "Host" attributes)
                                              (handle-request version attributes)
                                              (bad-request-error connection version)))
                              (else (bad-request-error connection version)))))
                         ((#f)
                          ; this is an HTTP/0.9 request
                          (handle-request 'HTTP/0.9 '()))
                         (else
                          (bad-request-error connection)))))

                   (define handle-request
                     (lambda (version attributes)
                       (let* ((method-table
                               (server-method-table hs))
                              (method-name
                               (vector-ref method-table method-index))
                              (method-action
                               (vector-ref method-table (+ method-index 1)))
                              (content
                               (read-content connection attributes))
                              (query
                               (let ((x (assoc "Content-Type" attributes)))
                                 (if (and x
                                          (string=?
                                           (cdr x)
                                           "application/x-www-form-urlencoded"))
                                     (decode-x-www-form-urlencoded content)
                                     (uri-query uri)))))
                         (let ((request
                                (make-request
                                 hs
                                 connection
                                 method-name
                                 uri
                                 version
                                 attributes
                                 query)))
                           (thread-specific-set! (current-thread) request))
                         (method-action))))

                   (cond ((not uri)
                          (bad-request-error connection))
                         ((not (< i (string-length req)))
                          (handle-version #f))
                         ((not (char=? (string-ref req i) #\space))
                          (bad-request-error connection))
                         (else
                          (let ((version-index
                                 (token-table-lookup-substring
                                  version-table
                                  req
                                  (+ i 1)
                                  (string-length req))))
                            (if version-index
                                (handle-version
                                 (vector-ref version-table
                                             (+ version-index 1)))
                                (bad-request-error connection)))))))))))))

(define version-table
  (make-token-table
   ("HTTP/1.0" 'HTTP/1.0)
   ("HTTP/1.1" 'HTTP/1.1)))

(define read-header
  (lambda (connection)
    (let loop ((attributes '()))
      (let ((line (permissive-read-line connection)))
        (cond ((not line)
               #f)
              ((= (string-length line) 0)
               attributes)
              (else
               (let ((attribute (split-attribute-line line)))
                 (if attribute
                     (loop (cons attribute attributes))
                     #f))))))))

(define read-content
  (lambda (connection attributes)
    (let ((cl
           (cond ((assoc "Content-Length" attributes)
                  =>
                  (lambda (x)
                    (let ((n (string->number (cdr x))))
                      (and n (integer? n) (exact? n) n))))
                 (else
                  #f))))
      (if cl
          (let ((str (make-string cl)))
            (let ((n (read-substring str 0 cl connection)))
              (if (= n cl)
                  str
                  "")))
          ""))))

(define permissive-read-line
  (lambda (port)
    (let ((s (read-line port)))
      (if (and (string? s)
               (> (string-length s) 0)
               (char=? (string-ref s (- (string-length s) 1)) #\return))
          ; efficient version of (substring s 0 (- (string-length s) 1))
          (begin (##string-shrink! s (- (string-length s) 1)) s)
          s))))

(define find-char-pos
  (lambda (str char)
    (let loop ((i 0))
      (if (< i (string-length str))
          (if (char=? char (string-ref str i))
              i
              (loop (+ i 1)))
          #f))))

(define split-attribute-line
  (lambda (line)
    (let ((pos (find-char-pos line #\:)))
      (and pos
           (< (+ pos 1) (string-length line))
           (char=? #\space (string-ref line (+ pos 1)))
           (cons (substring line 0 pos)
                 (substring line (+ pos 2) (string-length line)))))))

;==============================================================================
