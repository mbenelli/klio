; Test
(include "../klio/buffmap.scm")

(define buffer (make-random-u8vector 1024))
(define datamap (make-datamap 200))
(define get (build-accessors buffer datamap))

