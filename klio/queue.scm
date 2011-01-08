; Simple functional queues
;
; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
; All right reserved.


(##namespace ("queue#"))
(##include "~~lib/gambit#.scm")

(define-type queue front rear
  id: 06b8a1f7-e230-4590-b427-9efdf0384356
  constructor: %queue)


(define make-queue
  (lambda ()
    (%queue '() '())))


(define empty?
  (lambda (q)
    (and (null? (queue-front q)) (null? (queue-rear q)))))

(define empty-queue "Empty queue")

(define snoc
  (lambda (q x)
    (let ((f (queue-front q))
          (r (queue-rear q)))
      (if (null? f)
          (%queue (cons x '()) '())
          (%queue f (cons x r))))))


(define head
  (lambda (q)
    (if (empty? q)
        (raise empty-queue)
        (car (queue-front q)))))

(define tail
  (lambda (q)
    (if (empty? q)
        (raise empty-queue)
        (let ((f (queue-front q))
              (r (queue-rear q)))
          (if (= 1 (length f))
              (%queue (reverse r) '())
              (%queue (cdr f) r))))))


