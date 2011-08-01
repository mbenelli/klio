;; prelude.scm - A prelude for Gambit-C scheme.
;;
;; Copyright (c) 2008-2010 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.


(namespace
 ("prelude#"
  identity
  const
  compose
  str
  cout
  cerr
  nl
  ascii->char
  ucscode->char
  char-return
  char-tab
  char-newline
  parser-error
  split
  memoize

  with-gensyms
  when
  unless
  inc!
  dec!
  push!
  pop!
  assert
  and-let*
  cut
  cute
  with-mutex
))


                                        ; Macro utils

(define-macro (with-gensyms syms . body)
  `(let ,(map (lambda (x) (list x '(gensym))) syms)
     ,@body))


                                        ; Control

(define-macro (when test . body)
  `(if ,test (begin ,@body)))

(define-macro (unless test . body)
  `(if (not ,test) (begin ,@body)))


                                        ; Mutations


(define-macro (inc! x)
  `(begin (set! ,x (fx+ ,x 1)) ,x))

(define-macro (dec! x)
  `(begin (set! ,x (fx- ,x 1)) ,x))

(define-macro (push! x xs)
  `(set! ,xs (cons ,x ,xs)))

(define-macro (pop! xs)
  `(if (null? ,xs)
       (raise "pop! : empty list")
       (let ((x (car ,xs)))
         (set! ,xs (cdr ,xs))
         x)))

                                        ; Assert

(define-macro (assert . exprs)
  `(cond
     ((and ,@exprs) => (lambda (x) x))
     (else (raise (with-output-to-string "Assertion failure: "
                    (lambda () (display ',exprs)))))))


                                        ; Srfi-2 reference implementation

(define-macro (and-let* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))

    (##define-macro (ct-error-syntax msg . args)
      `(raise (with-output-to-string ,msg (lambda () (println ,@args)))))


    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (ct-error-syntax "bindings must be a list " bindings))
    (for-each
      (lambda (claw)
        (cond
          ((symbol? claw)	; BOUND-VARIABLE form
            (andjoin! claw))
          ((and (pair? claw) (null? (cdr claw))) ; (EXPRESSION) form
            (andjoin! (car claw)))
						; (VARIABLE EXPRESSION) form
          ((and (pair? claw) (symbol? (car claw))
              (pair? (cdr claw)) (null? (cddr claw)))
            (let* ((var (car claw)) (var-cell (cons var '())))
              (if (memq var new-vars)
                (ct-error-syntax "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (ct-error-syntax
              "An ill-formed binding in a syntactic form and-let* "
              claw))
        ))
      claws)
    (if (not (null? body))
        (andjoin! `(begin ,@body)))
    result))



                                        ; srfi-26 (cut)
                                        ;
                                        ; TODO: check conformance with srfi in
                                        ; corner cases, as:
                                        ; (cut <>)
                                        ; (cut + 2 3 4)

(define-macro (cut . args)
  (let loop ((bindings '()) (body '()) (args args))
    (cond
     ((null? args) `(lambda ,(reverse bindings) ,(reverse body)))
     ((eq? '|<>| (car args)) (let ((g (gensym)))
			       (loop (cons g bindings)
				     (cons g body)
				     (cdr args))))
     (else (loop bindings (cons (car args) body) (cdr args))))))


(define-macro (cute fn . args)
  (let loop ((bindings '()) (env '()) (body (list fn)) (args args))
    (cond
     ((null? args) `(lambda ,(reverse bindings)
		      (let ,env
			,(reverse body))))
     ((eq? '|<>| (car args)) (let ((g (gensym)))
			       (loop (cons g bindings)
				     env
				     (cons g body)
				     (cdr args))))
     (else (let ((g (gensym)))
	     (loop bindings
		   (cons (list g (car args)) env)
		   (cons g body)
		   (cdr args)))))))


                                       ; Simple mutex handlings.

(define-macro (with-mutex mutex . body)
  (let ((m (gensym))
        (r (gensym)))
    `(let ((,m ,mutex))
       (mutex-lock! ,m #f #f)
       (let ((,r (begin ,@body)))
         (mutex-unlock! ,m)
	 ,r))))

