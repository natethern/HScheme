"Loading Prelude.pure.scm"
(define define-syntax define)
(define let-syntax let)
;(define letrec-syntax letrec)

(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define caaar (lambda (x) (car (car (car x)))))
(define caadr (lambda (x) (car (car (cdr x)))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cdaar (lambda (x) (cdr (car (car x)))))
(define cdadr (lambda (x) (cdr (car (cdr x)))))
(define cddar (lambda (x) (cdr (cdr (car x)))))
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))

(define caaaar (lambda (x) (car (car (car (car x))))))
(define caaadr (lambda (x) (car (car (car (cdr x))))))
(define caadar (lambda (x) (car (car (cdr (car x))))))
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar (lambda (x) (car (cdr (car (car x))))))
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

; 4.2.1 Conditionals
(define cond (syntax-rules (=> else)
	((cond) <nothing>)
	((cond (else    . exprs)) (begin . exprs))
	((cond (test    . exprs) . rest) (if test (begin . exprs) (cond . rest)))
	((cond (test => . exprs) . rest) (if test (begin . exprs) (cond . rest)))
))

(define case (syntax-rules (else)
	((case key) <nothing>)
	((case key (else . exprs)) (begin . exprs))
	((case key (() . exprs) . rest) (case key . rest))
	((case key ((datum . data) . exprs) . rest) (if (eqv? key 'datum) (begin . exprs) (case key (data . exprs) . rest)))
))

(define and (syntax-rules ()
	((and) #t)
	((and first . rest) (if first (and . rest) #f))
))

(define or (syntax-rules ()
	((or) #f)
	((or first . rest) (if first #t (or . rest)))
))

; 6.2.5 Numerical Operations
(define complex? number?)

(define real? (lambda (n)
	(and (complex? n) (zero? (imag-part n)))
))

(define rational? (lambda (n)
	(and (real? n) (exact? n))
))

; 6.3.2 Pairs and Lists
(define reverse (lambda (x)       
	(if (null? x) x (append (reverse (cdr x)) (list (car x))))
))

(define list-tail (lambda (x k)
	(if (zero? k) x (list-tail (cdr x) (- k 1)))
))

(define list-ref (lambda (x k)
	(car (list-tail x k))
))

(define memq (lambda (item list)
	(if (null? list) #f
		(if (eq? item (car list)) list
			(memq item (cdr list))
		)
	)
))

(define memv (lambda (item list)
	(if (null? list) #f
		(if (eqv? item (car list)) list
			(memv item (cdr list))
		)
	)
))

(define member (lambda (item list)
	(if (null? list) #f
		(if (equal? item (car list)) list
			(member item (cdr list))
		)
	)
))

(define assq (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k v) . rest) (if (eq? k key) v (assq key rest)))
	)
))

(define assv (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k v) . rest) (if (eqv? k key) v (assv key rest)))
	)
))

(define assoc (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k v) . rest) (if (eqv? k key) v (assoc key rest)))
	)
))

; 6.4 Control Features
(define call-with-values (lambda (producer consumer)
	(apply consumer (values->list (producer)))
))

;
(define _null-environment (current-environment))
(define null-environment (lambda () _null-environment))

"Prelude.pure.scm Loaded"
