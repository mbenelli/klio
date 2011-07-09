; buffmap.scm - Build variables from a buffer following predefined definition.
;
; Copyright (c) 2011 by Benelli Marco <mbenelli@yahoo.com>.
; All Right Reserved.
;
; These utilities builds functionis to read and write variables.
; A typical use is in conjunction with a binary communication protocol (ie:
; modbus), the variables are read from a buffer and written using a function
; provided by client.
;
; TODO: This is actually a work in progress and need substantial testing.
; 

(##namespace ("buffmap#"))
(##include "~~lib/gambit#.scm")
(##namespace ("floats#" read-f32 read-f64))


                                        ; TODO: following functions are for
					; testing purpose.
					; This testing code is out of date,
					; because it use alists, superseeded
					; by list of variables.

; Initialize an u8vector with random integers between 0 and 255

(define (make-random-u8vector n)
  (let ((v (make-u8vector n)))
    (do ((i 0 (+ i 1)))
      ((= i (u8vector-length v)) v)
      (u8vector-set! v i (random-integer 255)))))


; Return a random value choosed from arguments.

(define (choose . choices)
  (let ((n (length choices)))
    (list-ref choices (random-integer n))))



; Return an alist of N variable names and offsets representing addresses of
; variables in a buffer.  Variables can be boolean or integer.
; (<name> <bit|byte> <offset>)

(define (make-datamap n)
  (let ((make-name (lambda (x)
		     (string->symbol
		       (with-output-to-string "v"
			 (lambda () (display x)))))))
    (let loop ((i 0)
	       (offset 0)
	       (bits (choose 0 1 2 3 4 5 6 7 #f))
	       (datamap '()))
      (cond
       ((> i n) (reverse datamap))
       (bits => (lambda (b)
                  (loop (+ i 1)
                        (+ offset 1)
                        (if (zero? b) (choose 7 #f) (- b 1))
                        (cons
			  (make-var (make-name i) 'bit offset b)
                          datamap))))
       (else (loop (+ i 1) (+ offset 8) (choose 7 #f)
                   (cons
		     (make-var (make-name i) 'byte offset)
		     datamap)))))))


                                       ; Real code stars here

(define types '(bit byte f32 f64))

(define-type
  var
  id: F32C84CD-66FA-4E85-920E-13C4560DDA55
  constructor: %make-var
  name
  type
  offset
  bit-index)

(define (make-var name type offset #!optional (bit-index #f))
  (cond
    ((and (eq? type 'bit)
	  (integer? bit-index)
	  (and (>= bit-index 0)
	       (< bit-index 8))) (%make-var name type offset bit-index))
    ((not (memq type types)) (raise "Unknow type"))
    (else (%make-var name type offset #f))))

; Build an hash table of accessor to BUFFER with mapping defined by DATAMAP,
; where DATAMAP is a list of variables as defined by make-var.
;
; Usage: ((table-ref accessors 'v0))

(define (build-accessors buffer datamap)
  (let ((accessors (make-table)))
    (for-each
     (lambda (x)
       (let ((name (var-name x))
             (type (var-type x))
             (offset (var-offset x)))
	 (cond
	   ((eq? type 'byte)
	    (table-set! accessors name
			(lambda ()
			  (u8vector-ref buffer offset))))
	   ((eq? type 'bit)
	    (table-set! accessors name
			(lambda ()
			  (extract-bit-field
			    1
			    (var-bit-index x)
			    (u8vector-ref buffer offset)))))
	   ((eq? type 'f32)
	    (table-set! accessors name
			(lambda ()
			  (with-input-from-u8vector
			    (subu8vector buffer offset (+ offset 4))
			    (lambda () (read-f32 (current-input-port)))))))
	   ((eq? type 'f64)
	    (table-set! accessors name
			(lambda ()
			  (with-input-from-u8vector
			    (subu8vector buffer offset (+ offset 8))
			    (lambda () (read-f64 (current-input-port)))))))
	   (else (raise "Unknown type")))))
     datamap)
    (lambda (k) ((table-ref accessors k)))))


; Build a table of mutators based on DATAMAP.
; FN is a function that consumes a type and an offset and produce a function
; that consume the value to be set.

(define (build-mutators datamap fn)
  (let ((mutators (make-table)))
    (for-each
      (lambda (x)
        (let ((name (car x))
              (type (cadr x))
              (offset (caddr x)))
          (table-set! mutators name (fn type offset))))
      datamap)
    (lambda (k v) ((table-ref mutators k) v))))

