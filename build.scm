;; Copyright (c) 2010 Marco Benelli <mbenelli@yahoo.com> . All right reserved.
;;
;; Build file for klio.

(define dir "klio")

(define srcs
  '(
    prelude
    lists
    charsets
    strings
    irregex
    datetime
    rfc1123
    format
    vectors
    simple-sort
    shift-reset

    queue
    buffmap
    logger

    base64
    uri
    http-srv
    json

    input-parse
    ssax
    sxml

    kws
    http-auth
    sessions
    cgi

    modbus
    fetchwrite
    ))

(define (compile-sqlite)
  (with-exception-catcher
    (lambda ()
      (println "WARNING: sqlite3 not compiled."))
    (lambda ()
      (let ((sqlite-flags (with-input-from-process
                           '(path: "pkg-config" arguments: ("--cflags" "sqlite3"))
                           read-line))
           (sqlite-libs (with-input-from-process
                          '(path: "pkg-config" arguments: ("--libs" "sqlite3"))
                          read-line)))
       (if (file-exists? "sqlite3.o1")
           (delete-file "sqlite3.o1"))
       (print "Compiling sqlite3 (warnings disabled)... ")
       (compile-file "sqlite3.scm"
         cc-options: (string-append "-w " sqlite-flags)
         ld-options: sqlite-libs)
       (println "done.")))))

(parameterize ((current-directory dir))
  (for-each
    (lambda (x)
      (let* ((name (symbol->string x))
             (src (string-append name ".scm"))
             (obj (string-append name ".o1")))
        (if (file-exists? obj)
            (delete-file obj))
        (print "Compiling " x " ... ")
        (compile-file src)
        (println "done.")))
    srcs)
  (compile-sqlite)
  )

