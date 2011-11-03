; Test

(##namespace ("buffmap-test#"))
(##include "~~lib/gambit#.scm")
(##include "../klio/test.scm")

(load "../klio/ctypes")
(load "../klio/buffmap")

(##namespace
 ("buffmap#" make-var build-accessors))


(define buffer
  (u8vector              ; type    little-endian    big-endian
    #x00                 ; u8            0             0
    #xff                 ; u8          255           255
    #xfe                 ; bits              01111111
    #x01                 ; u8            1             1
    0 0 128 63           ; f32           1.0           4.600602988224807e-41
    #x02                 ; u8            2             2
    #x03                 ; u8            3             3
    0 0 0 0 0 0 240 63)) ; f64           1.0           3.03865e-319
  
(define datamap
  (map
    (lambda (x)
      (apply make-var x))
    `((v0 byte 0)
      (v1 byte 1)
      (v2 bit 2 0)
      (v3 bit 2 1)
      (v4 bit 2 2)
      (v5 bit 2 3)
      (v6 bit 2 4)
      (v7 bit 2 5)
      (v8 bit 2 6)
      (v9 bit 2 7)
      (v10 byte 3)
      (v11 f32 4)
      (v12 byte 8)
      (v13 byte 9)
      (v14 f64 10))))

(define get (build-accessors buffer datamap))
(define get-be (build-accessors buffer datamap 'big))

(define (run)
  (test (get 'v0) 0)
  (test (get 'v1) 255)
  (test (get 'v2) 0)
  (test (get 'v3) 1)
  (test (get 'v4) 1)
  (test (get 'v5) 1)
  (test (get 'v6) 1)
  (test (get 'v7) 1)
  (test (get 'v8) 1)
  (test (get 'v9) 1)
  (test (get 'v10) 1)
  (test (get 'v11) 1.0)
  (test (get 'v12) 2)
  (test (get 'v13) 3)
  (test (get 'v14) 1.0)

  (test (get-be 'v11) 4.600602988224807e-41)
  (test (get-be 'v14) 3.03865e-319))

(run)

