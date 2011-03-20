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
    format
    vectors

    base64
    http
    json

    input-parse
    ssax
    sxml

    kws
    ))

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
    srcs))

