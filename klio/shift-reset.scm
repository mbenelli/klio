; shift-reset.scm - Static delimited continuation.
;
; The code is taken from:
; http://okmij.org/ftp/Scheme/delimcc-simple.scm
;
; Modifications:
; 
; Modified to use Gambit's define-macro instead of syntax-rules.
; The validation code has been removed.

(##namespace ("shift-reset#"))
(##include "~~lib/gambit#.scm")

; ----------------------------------------------------------------------------

; The implementation of ordinary shift/reset derived 
; by simplifying multi-prompt shift/reset in delimcc.scm
;
; Although the present code should work on any R5RS Scheme system,
; good performance should be expected only on the systems that implement
; call/cc efficiently, such as Chez Scheme, Scheme48, Gambit, Larceny.
;
; Even on systems that support call/cc only inefficiently,
; this implementation has an advantage of not leaking memory.
; The captured continuation, reified by shift, corresponds only
; to the needed prefix of the full continuation, _even_
; if call/cc copies the whole stack. In other words, this implementation
; has a so-called JAR hack (see shift-reset.scm in Scheme48 distribution)
; built in. Please see the memory-leak test at the end.


(define go #f)

; pstack is a list of k: stack fragments
(define pstack '())

; Execute a thunk in the empty environment -- at the bottom of the stack --
; and pass the result, too encapsulated as a thunk, to the
; continuation at the top of pstack. The top-most pstack frame is
; removed.
;
; We rely on the insight that the capture of a delimited continuation
; can be reduced to the capture of the undelimited one. We invoke 
; (go th) to execute the thunk th in the delimited context. 
; The call to 'go' is evaluated almost in the empty context
; (near the `bottom of the stack'). Therefore,
; any call/cc operation encountered during the evaluation of th
; will capture at most the context established by the 'go' call, NOT
; including the context of go's caller. Informally, invoking (go th)
; creates a new stack segment; continuations captured by call/cc
; cannot span the segment boundaries, and are hence delimited.
; 
; This emulation of delimited control is efficient providing that
; call/cc is implemented efficiently, with the hybrid heap/stack or
; stack segment strategies.

(let ((v
	(call/cc
	  (lambda (k)
	    (set! go k)
	    (k #f)))))
  (if v
    (let* ((r (v))
	   (h (car pstack))
	   (_ (set! pstack (cdr pstack))))
      (h r))	; does not return
    ))

;; let push_prompt_aux (p : 'a prompt) (body : unit -> 'a) : 'a =
;;   let ek = get_ek () in
;;   let pframe = {pfr_mark = p.mark; pfr_ek = ek} in
;;   let () = ptop := pframe :: (!ptop) in
;;   let res = body () in
;;   let () = p.mbox := fun () -> res in
;;   raise DelimCCE

(define (reset* th)
  (call/cc
    (lambda (k)
       (set! pstack (cons k pstack))
       (go th))))			; does not return

(define (shift* f)
  (call/cc
    (lambda (k)			; stack fragment
      (go 
	(lambda () 
	  (f 
	    (lambda (v)
	      (call/cc (lambda (k1)
			 (set! pstack (cons k1 pstack))
			 (k v))))))))))

; ------------------------------- Syntactic sugar

;(define-syntax reset
;  (syntax-rules ()
;    ((_ ?e ?f ...) (reset* (lambda () ?e ?f ...)))))
;
;(define-syntax shift
;  (syntax-rules ()
;    ((_ ?k ?e ?f ...) (shift* (lambda (?k) ?e ?f ...)))))

