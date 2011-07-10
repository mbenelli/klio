(include "../ctypes#.scm")
(include "../ctypes.scm")

(call-with-output-file
  "f32test"
  (lambda (port)
    (write-f32 -1.5 port)
    (write-f32 +inf.0 port)
    (write-f32 3.1415926 port)))

(call-with-input-file
  "f32test"
  (lambda (port)
    (let* ((a (read-f32 port))
           (b (read-f32 port))
           (c (read-f32 port))
           (d (read-f32 port)))
      (pp (list a b c d)))))


(call-with-output-file
  "f64test"
  (lambda (port)
    (write-f64 -1.5 port)
    (write-f64 +inf.0 port)
    (write-f64 3.1415926 port)))

(call-with-input-file
  "f64test"
  (lambda (port)
    (let* ((a (read-f64 port))
           (b (read-f64 port))
           (c (read-f64 port))
           (d (read-f64 port)))
      (pp (list a b c d)))))

