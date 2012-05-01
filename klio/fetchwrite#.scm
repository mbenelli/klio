;; fetchwrite#.scm - fetchwrite namespaces.
;;
;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Right Reserved.
;;
;; Author: Marco Benelli <mbenelli@yahoo.com>
;;
;; The fecth-write protocol is used to communicate with Siemens S5/S7 plcs.

(##namespace
  ("fetchwrite#"

   DB
   M
   I
   Q
   PI-PQ
   C
   T

   OPCODE-WRITE
   OPCODE-FETCH

   OK
   ERROR
   ERROR-INVALID-PARAM
   ERROR-CONNECTION
   ERROR-TIMEOUT
   ERROR-COMUNICATION
   ERROR-BUFFER
   ERROR-SEND
   ERROR-RECV

   make-request-header
   make-response-header
   fetch-db
   write-db
   fetch/apply))

