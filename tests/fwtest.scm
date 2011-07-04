;;; fwtest.scm - Test for fetch-write protocol.
;;;
;;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;;; All Right Reserved.
;;;
;;; Author: Marco Benelli <mbenelli@yahoo.com>
;;;

(##namespace ("fwtest#"))
(##include "~~lib/gambit#.scm")
(##include "../klio/fetchwrite#.scm")

(define fetch-port
  (open-tcp-client
    `(server-address: "192.168.1.32" port-number: 2000)))

(define res (fetch-db 62 0 250 fetch-port))

(pp res)
(close-port fetch-port)

