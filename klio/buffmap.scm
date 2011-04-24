; buffmap.scm - Build variables from a buffer following predefined definition.
;
; Copyright (c) 2011 by Benelli Marco <mbenelli@yahoo.com>.
; All Right Reserved.
;
; These utilities builds function to read and write variables.
; A typical use is in conjunction with a binary communication protocol (ie:
; modbus), the variables are read from a buffer and written using a function
; provided by client.
;
; TODO: This is actually a work in progress and need sostantial testing.
; 

(##namespace ("buffmap#"))
(##include "~~lib/gambit#.scm")

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
  (let ((make-variable (lambda (x)
                         (string->symbol
                          (with-output-to-string "v"
                            (lambda () (display x)))))))
    (let loop ((i 0) (offset 0) (bits (choose 7 #f)) (datamap '()))
      (cond
       ((> i n) (reverse datamap))
       (bits => (lambda (b)
                  (loop (+ i 1)
                        (+ offset 1)
                        (if (zero? b) (choose 7 #f) (- b 1))
                        (cons
                          (list (make-variable i) 'bit offset)
                          datamap))))
       (else (loop (+ i 1) (+ offset 8) (choose 7 #f)
                   (cons (list (make-variable i)
                               'byte
                               offset)
                         datamap)))))))


; Build an hash table of accessor to BUFFER with mapping defined by DATAMAP.
; Usage: ((table-ref accessors 'v0))

(define (build-accessors buffer datamap)
  (let ((accessors (make-table)))
    (for-each
     (lambda (x)
       (let ((name (car x))
             (type (cadr x))
             (offset (caddr x)))
         (if (eq? type 'byte)
             (table-set! accessors name
                         (lambda ()
                           (u8vector-ref buffer (quotient offset 8))))
             (let ((index (quotient offset 8))
                   (bit (remainder offset 8)))
               (table-set! accessors name
                           (lambda ()
                             (extract-bit-field
                               1
                               bit
                               (u8vector-ref buffer index))))))))
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

