; Test

(##namespace ("buffmap-test#"))
(##include "~~lib/gambit#.scm")
(##namespace
 ("buffmap#" make-var build-accessors))

(define buffer
  (u8vector
    #x00
    #xff
    #xff
    #x01
    0 0 128 63   ; f32 1.0
    #x02
    #x03
    0 0 0 0 0 0 240 63)) ; f64 1.0  
  
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

