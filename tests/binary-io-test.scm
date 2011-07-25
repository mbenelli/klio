;;; Basic test suite for SRFI-56 Binary I/O.

;(include "srfi-56.scm") ; for testing Gambit/Chicken compilers

(##namespace ("test-binary-io#"))
(##include "~~lib/gambit#.scm")
(##include "../klio/binary-io#.scm")

;; set to #t to only report failures
(define silent-pass? #f)
;; approx matching to account for round-off errors
(define default-epsilon 0.00001)

;; faster but unsafe binary string-port method
; (define (do-read/write n reader writer)
;   (with-input-from-string
;       (with-output-to-string (lambda () (writer n)))
;     reader))

;; temp file method, assumes writable /tmp/ dir
(define do-read/write
  (let ((file "tmp-binary-io.dat"))
    (lambda (n reader writer)
      ;; this is needed in MzScheme and Chez
      (if (file-exists? file)
        (delete-file file))
      (with-output-to-binary-file file (lambda () (writer n)))
      (with-input-from-binary-file file reader))))

(define fail-count 0)
(define pass-count 1)

(define (fail msg n res)
  (set! fail-count (+ fail-count 1))
  (display "fail: ")
  (display msg)
  (display " expected ")
  (write n)
  (display " got ")
  (write res)
  (newline))

(define (approx msg n res)
  (set! pass-count (+ pass-count 1))
  (if (not silent-pass?)
    (begin
      (display "close-enough: ")
      (display msg)
      (display " ")
      (write res)
      (display " is almost ")
      (write n)
      (newline))))

(define (pass msg res)
  (set! pass-count (+ pass-count 1))
  (if (not silent-pass?)
    (begin
      (display "pass: ")
      (display msg)
      (display " ")
      (write res)
      (newline))))

(define (run-tests vary-endian? reader writer msg ls . o)
  (let ((epsilon (and (pair? o) (car o))))
    (for-each
     (lambda (n)
       (let ((p (current-output-port))
             (one (lambda (r w . variant)
                    (let ((res (do-read/write n r w))
                          (msg2 (if (null? variant)
                                  msg
                                  (string-append msg (car variant)))))
                      (if (not (eqv? n res))
                        (if (and epsilon
                                 (number? n) (number? res)
                                 (not (zero? res))
                                 (< (abs (- 1 (/ n res))) epsilon))
                          (approx msg2 n res)
                          (fail msg2 n res))
                        (pass msg2 res))))))
         (one reader writer)
         (cond (vary-endian?
                (one (lambda ( ) (reader   (current-input-port)    'big-endian))
                     (lambda (n) (writer n (current-output-port)    'big-endian))
                     " (big-endian)")
                (one (lambda ( ) (reader   (current-input-port) 'little-endian))
                     (lambda (n) (writer n (current-output-port) 'little-endian))
                     " (little-endian)")))))
     ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixnums

(run-tests #f read-binary-uint8 write-binary-uint8
  "read/write-binary-uint8"
  '(0 1 23 31 32 127 128 255))

(run-tests #f read-binary-sint8 write-binary-sint8
  "read/write-binary-sint8"
  '(-128 -127 -23 -1 0 1 23 31 32 127))

(run-tests #t read-binary-uint16 write-binary-uint16
  "read/write-binary-uint16"
  '(0 1 23 31 32 127 128 255 256 65535))

(run-tests #t read-binary-sint16 write-binary-sint16
  "read/write-binary-sint16"
  '(-32768 -32767 -256 -255 -128 -127 -23 -1 0 1 23 31 32 127 128 255 256 32767))


;; fixed size, possibly bignum or unsupported

(run-tests #t read-binary-uint32 write-binary-uint32
  "read/write-binary-uint32"
  '(0 1 23 31 32 127 128 255 256 65535 65536
    2147483648 4294967295 ; comment out for impls w/o bignums
    ))

(run-tests #t read-binary-sint32 write-binary-sint32
  "read/write-binary-sint32"
  '(-2147483648
    -65536 -65535 -32768 -32767 -256 -255 -128 -127 -23 -1
    0 1 23 31 32 127 128 255 256 32767
    65535 65536 2147483647 ; comment out for impls w/o bignums
    ))

(run-tests #t read-binary-uint64 write-binary-uint64
  "read/write-binary-uint64"
  '(0 1 23 31 32 127 128 255 256 65535 65536
    2147483648 4294967295 4294967296
    9223372036854775807 18446744073709551615
    ))

(run-tests #t read-binary-sint64 write-binary-sint64
  "read/write-binary-sint64"
  '(-4294967296 -2147483648 -65536
    -65535 -32768 -32767 -256 -255 -128 -127 -23 -1
    0 1 23 31 32 127 128 255 256 32767 ;65535 65536
    2147483647 4294967295 4294967296 9223372036854775807
    ))


;; bignums

(run-tests #f read-ber-integer write-ber-integer
  "read/write-ber-integer"
  '(0 1 128 16383 32767
    18446744073709551615 340282366920938463463374607431768211456
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; floating point - these may not pass depending on the internal
;; precision of your Scheme

(run-tests #t read-ieee-float32 write-ieee-float32
  "read/write-ieee-float32"
  `(0.0 1.0 -1.0 0.333333333 1/3
    1.192092896e-7 ,(+ 1 1.192092896e-7)
    1e-23 -1e-23 3.40282346638528860e+38 -3.40282346638528860e+38
    1.40129846432481707e-45 -1.40129846432481707e-45
    3.14159265358979323846
    )
  default-epsilon)

(run-tests #t read-ieee-float64 write-ieee-float64
  "read/write-ieee-float64"
  `(0.0 1.0 -1.0 0.333333333 1/3
    1.192092896e-7 ,(+ 1 1.192092896e-7) 1e-23 -1e-23
    1.7976931348623157E+308 -1.7976931348623157E+308
    4.9406564584124654E-324 -4.9406564584124654E-324
    3.14159265358979323846
    )
  default-epsilon)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing incomplete reads

(for-each
 (lambda (reader name)
   (let ((res (do-read/write 0 reader (lambda (n) (write-u8 0)))))
     (cond
       ((eof-object? res)
        (set! pass-count (+ pass-count 1))
        (display "pass: incomplete/"))
       (else
        (set! fail-count (+ fail-count 1))
        (display "fail: incomplete/")))
     (display name)
     (newline)))
 (list read-binary-uint16 read-binary-uint32 read-binary-sint16 read-binary-sint32
       read-network-uint16 read-network-uint32 read-ieee-float32 read-ieee-float64)
 '(read-binary-uint16 read-binary-uint32 read-binary-sint16 read-binary-sint32
   read-network-uint16 read-network-uint32 read-ieee-float32 read-ieee-float64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The above tests confirm read/write invariance, but does not
;; necessarily guarantee we are really using the representation the
;; machine would use.  We include a little-endian data file generated
;; from C code to confirm true IEEE-754 encodings.

(define (run-read-test name reader x port . o)
  (let ((epsilon (and (pair? o) (car o))))
    (let ((y (reader port 'little-endian)))
      (cond
        ((eqv? x y) (pass name x))
        ((and epsilon (number? x) (number? y) (not (zero? y))
              (< (abs (- 1 (/ x y))) epsilon))
         (approx name x y))
        (else (fail name x y))))))

(call-with-binary-input-file "binary-io-test.dat"
  (lambda (p)
    ;; read-ieee-float32
    (for-each
     (lambda (x)
       (run-read-test "read-ieee-float32/static-data" read-ieee-float32 x p default-epsilon))
     `(0.0 -1.0 1/3
       1.192092896E-07 ,(+ 1 1.192092896E-07) 1e-23 -1e-23
       3.40282346638528860e+38 -3.40282346638528860e+38
       1.40129846432481707e-45 -1.40129846432481707e-45
       3.14159265358979323846))
    ;; read-ieee-float64
    (for-each
     (lambda (x)
       (run-read-test "read-ieee-float64/static-data" read-ieee-float64 x p default-epsilon))
     `(0.0 -1.0 1/3
       1.192092896E-07 ,(+ 1 1.192092896E-07) 1e-23 -1e-23
       1.7976931348623157E+308 -1.7976931348623157E+308
       4.9406564584124654E-324 -4.9406564584124654E-324
       3.14159265358979323846))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; final report

(newline)
(display pass-count)
(display " tests passed.")
(newline)
(display fail-count)
(display " tests failed.")
(newline)

