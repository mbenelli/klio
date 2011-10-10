;; kliows.scm - Klio web server launcher
;;
;; Copyright (c) 2010, 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(define (main . args)
  (sessions#start-sessions-manager)
  (kws#kws port-number: 8000 server-root: "." multithread: #t))

(main)

