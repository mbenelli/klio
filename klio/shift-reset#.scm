; shift-reset#.scm - Static delimited continuation.

(namespace
  ("shift-reset#"
   reset*
   reset
   shift*
   shift))

(define-macro (reset e . rest)
  `(reset* (lambda () ,e ,@rest)))

(define-macro (shift k e . rest)
  `(shift* (lambda (,k) ,e ,@rest)))

