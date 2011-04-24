; shift-reset-test.scm - Test of static delimited continuation.
;
; The code is taken from:
; http://okmij.org/ftp/Scheme/delimcc-simple.scm
;

(include "../klio/shift-reset#.scm")

; ------------------------------- Tests

(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (* 10 (reset (* 2 (shift g (* 5 (shift f (+ (f 1) 1))))))))
(newline)
; --> 60

(display (let ((f (lambda (x) (shift k (k (k x))))))
	   (+ 1 (reset (+ 10 (f 100))))))
(newline)
; --> 121

; shift f1 tests that we implement shift rather than shift0
(display (reset
	   (let ((x (shift f 
		      (shift f1 (f1 (cons 'a (f '())))))))
	     (shift g x))))
(newline)
; ==> '(a)

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(reset (display (let ((x 'abcde)) (eq? x ((shift* shift*) x)))))
(newline)

(define traverse
  (lambda (xs)
    (letrec ((visit
	       (lambda (xs)
		 (if (null? xs)
		   '()
		   (visit (shift*
			    (lambda (k)
			      (cons (car xs) (k (cdr xs))))))))))
      (reset*
	(lambda ()
	  (visit xs))))))

(display "Ex by Olivier Danvy") (newline)
(display (traverse '(1 2 3 4 5)))
(newline)


; Testing garbage-retention in Petite Chez Scheme
; Using guardians
; For explanations: http://www.scheme.com/csug/smgmt.html#g2352
; This memory leak test is due to Chung-chieh Shan.
; This test can be adjusted to run on any other system:
; it should loop forever in constant memory. In fact, it was first
; written in portable Scheme; guardians were added later.

(define (test-gc)
  (let ((g (make-guardian)))
    (let loop ((junk-identity
		 (let ((junk (list 'junk)))
		   (cons junk (reset (shift f f)))))
		(done 10))
      (if (zero? done)
	(begin
	  (collect (collect-maximum-generation)) ; force all collection
	  (display "checking if junk became inacessible:") (newline)
	  (do ((junk-inaccessible (g) (g))) ((not junk-inaccessible))
	    (display "collected junk of size: ")
	    (display junk-inaccessible)
	    (newline)))
	(begin
	  (g (car junk-identity)) ; register with the guardian
	  (set-cdr! (car junk-identity)
	    (list (cdr junk-identity)))
	  (loop (cons (cdr (car junk-identity))
		  (cdr junk-identity)) (- done 1)))))))


;; > (test-gc)
;; checking if junk became inacessible:
;; collected junk of size: (junk #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure> #<procedure>)
;; collected junk of size: (#<procedure> #<procedure>)
;;

; This listing shows that the junk is collected rather than retained.
; In contrast, the original implementation of shift/reset in terms of
; call/cc collects nothing: all junk is retained.


; Another leak test
(define (leak-test1-g identity-thunk)
  (let ((g (make-guardian)))
    (let loop ((id (lambda (x) x)) (done 10))
      (if (zero? done)
	(begin
	  (collect (collect-maximum-generation)) ; force all collection
	  (display "collected pieces of junk: ")
	  (display
	    (do ((junk-inaccessible (g) (g)) (c 0 (+ 1 c)))
	        ((not junk-inaccessible) c)))
	    (newline))
	 (begin
	   (g id) ; register with the guardian
	   (loop (id (identity-thunk)) (- done 1)))))))

; (leak-test1-g (lambda () (reset (shift f f))))
; collected pieces of junk: 10
