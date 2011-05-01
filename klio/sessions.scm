; session.scm - User session handling
;
; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
; All Rights Reserved.
;
; Non standard dependencies: ##current-time-point

(##namespace ("sessions#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")
(##namespace ("lists#" member remove!))


(define now ##current-time-point)

(define session-timeout (make-parameter 10))

(define current-sessions '())

(define sessions-mutex (make-mutex))


(define-type session
  id: 4BF6A2DD-B55B-419B-9529-851550479B42
  constructor: %make-session
  id
  user
  since
  last)


(define (expired? s)
  (< (+ (session-last s) (session-timeout))
     (now)))


(define (session-by-key predicate)
  (lambda (key)
    (mutex-lock! sessions-mutex #f #f)
    (cond
      ((member key current-sessions predicate)
       => (lambda (x)
            (let ((s (car x)))
              (session-last-set! s (now))
              (mutex-unlock! sessions-mutex)
              s)))
      (else (mutex-unlock! sessions-mutex) #f))))


(define valid-session
  (session-by-key
    (lambda (id session) (eq? id (session-id session)))))


(define already-logged-in
  (session-by-key
    (lambda (user session) (string=? user (session-user session)))))


(define (authorized? user passwd)
  (and (string=? user "foo") (string=? passwd "bar")))


(define (make-session user passwd)
  (cond
    ((already-logged-in user) => values)
    ((authorized? user passwd) (let ((s (%make-session
                                          (random-integer 1000000)
                                          user
                                          (now)
                                          (now))))
				 (mutex-lock! sessions-mutex #f #f)
				 (push! s current-sessions)
				 (mutex-unlock! sessions-mutex)
                                 s))
    (else #f)))


(define (new-session user passwd)
  (cond
    ((make-session user passwd) => (lambda (x) (session-id x)))
    (else #f)))


(define (check-sessions!)
  (set! current-sessions (remove! expired? current-sessions)))

(define check-sessions-thread
  (make-thread
    (lambda ()
      (let ((start (time->seconds (current-time))))
        (let loop ((x (session-timeout)))
          (thread-sleep! (seconds->time (+ x start)))
          (mutex-lock! sessions-mutex #f #f)
          (check-sessions!)
          (mutex-unlock! sessions-mutex)
          (loop (+ x (session-timeout))))))))

(define (start-sessions-manager)
  (thread-start! check-sessions-thread))

(define (stop-sessions-manager)
  (thread-terminate! check-sessions-thread))

