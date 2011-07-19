;;; fwtest.scm - Test for fetch-write protocol.
;;;
;;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;;; All Right Reserved.
;;;
;;; Author: Marco Benelli <mbenelli@yahoo.com>
;;;

(##include "../klio/ctypes#.scm")
(##include "../klio/fetchwrite#.scm")

(define address (make-parameter "localhost"))

(define (test-fetch-db)
  (let* ((fp (open-tcp-client
               `(server-address: ,(address) port-number: 2000)))
         (res (fetch-db 62 0 250 fp)))
    (pp res)
    (close-port fp)))

; TODO: clean up, test alternatives and move all the endianess-aware code
; to ctypes.

(define native-endianess (make-parameter 'little))

(define (read-f32vector n port endianess)
  (let* ((res (make-f32vector n))
	 (float-buffer (make-u8vector 4))
	 (push-value
	   (if (eq? endianess (native-endianess))
	       (lambda (i)
		 (vector-set! res i (read-f32 port)))
	       (lambda (i)
		 (u8vector-set! float-buffer 3 (read-u8 port))
		 (u8vector-set! float-buffer 2 (read-u8 port))
		 (u8vector-set! float-buffer 1 (read-u8 port))
		 (u8vector-set! float-buffer 0 (read-u8 port))
		 (f32vector-set! res i (with-input-from-u8vector
					 float-buffer
					 read-f32))))))
    (do ((i 0 (+ i 1)))
      ((= i n) res)
      (push-value i))))
      

(define-type
  query0

  measures
  alarms
  enablings
  prms)


(define (read-db p)
  (let ((measures (make-vector 32))
	(alarms (make-u8vector 10))
	(enablings (make-u8vector 16))
	(prms (make-vector 24)))
    (set! measures (read-f32vector 32 p 'big))
    (read-subu8vector alarms 0 10 p)
    (read-subu8vector enablings 0 16 p)
    (set! prms (read-f32vector 24 p 'big))
    (make-query0 measures alarms enablings prms)))


(define (test-fetch/apply)
  (let ((p (open-tcp-client
	     `(server-address: ,(address) port-number: 2000))))
    (fetch/apply 62 0 250 read-db p)))

(define q0 (call-with-input-u8vector r read-db))

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

