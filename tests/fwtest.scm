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

;; (define fetch-port
;;   (open-tcp-client
;;     `(server-address: "192.168.1.32" port-number: 2000)))

;; (define res (fetch-db 62 0 250 fetch-port))

;; (pp res)
;; (close-port fetch-port)

(define (run)
  (let* ((fp (open-tcp-client
               `(server-address: "192.168.1.32" port-number: 2000)))
         (res (fetch-db 62 0 250 fp)))
    (pp res)
    (close-port fp)))


;; Sample output.

(define r
  '#u8(
                                        ; measure 0
       66
       242
       4
       108
                                        ; 1
       67
       159
       74
       131
                                        ; 2
       0
       0
       0
       0
                                        ; 3
       0
       0
       0
       0
                                        ; 4
       0
       0
       0
       0
                                        ; 5
       0
       0
       0
       0
                                        ; 6
       64
       224
       0
       0
                                        ; 7
       65
       166
       149
       142
                                        ; 8
       0
       0
       0
       0
                                        ; 9
       0
       0
       0
       0
                                        ; 10
       0
       0
       0
       0
                                        ; 11
       0
       0
       0
       0
                                        ; 12
       0
       0
       0
       0
                                        ; 13
       0
       0
       0
       0
                                        ; 14
       65
       1
       213
       9
                                        ; 15
       0
       0
       0
       0
                                        ; 16
       66
       242
       4
       108
                                        ; 17
       0
       0
       0
       0
                                        ; 18
       0
       0
       0
       0
                                        ; 19
       0
       0
       0
       0
                                        ; 20
       0
       0
       0
       0
                                        ; 21
       0
       0
       0
       0
                                        ; 22
       0
       0
       0
       0
                                        ; 23
       0
       0
       0
       0
                                        ; 24
       0
       0
       0
       0
                                        ; 25
       0
       0
       0
       0
                                        ; 26
       66
       242
       4
       119
                                        ; 27
       0
       0
       0
       0
                                        ; 28
       0
       0
       0
       0
                                        ; 29
       65
       152
       0
       0
                                        ; 30
       0
       0
       0
       0
                                        ; 31
       0
       0
       0
       0
                                        ; 
       0
       0
       0
       0
                                        ; Alarms
       0
       0
       0
       0
       0
       0
       9
       0
       0
       141
                                        ; misc
       0
       0
       
       11
       232
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       67
       22
       
       0
       0
       0
       0
       
       0
       0
       64
       64
       
       0
       0
       66
       72
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0
       0
       0
       
       0
       0))

