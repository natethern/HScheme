; 4.2.1 Conditionals
(define-syntax cond (syntax-rules (=> else)
	((cond) <nothing>)
	((cond (else    . exprs)) (begin . exprs))
	((cond (test    . exprs) . rest) (if test (begin . exprs) (cond . rest)))
	((cond (test => . exprs) . rest) (if test (begin . exprs) (cond . rest)))
))

(define-syntax case (syntax-rules (else)
	((case key) <nothing>)
	((case key (else . exprs)) (begin . exprs))
	((case key (() . exprs) . rest) (case key . rest))
	((case key ((datum . data) . exprs) . rest) (if (equal? key 'datum) (begin . exprs) (case key (data . exprs) . rest)))
))

(define-syntax and (syntax-rules ()
	((and) #t)
	((and first . rest) (if first (and . rest) #f))
))

(define-syntax or (syntax-rules ()
	((or) #f)
	((or first . rest) (if first #t (or . rest)))
))


; 4.2.6 Quasiquotation
(define-syntax quasiquote (syntax-rules (unquote unquote-splicing quasiquote)
	((quasiquote (unquote a)) a)
	((quasiquote (quasiquote a)) (quote (quasiquote a)))
	((quasiquote ((unquote-splicing a) . b)) (append a (quasiquote b)))
	((quasiquote (a . b)) (cons (quasiquote a) (quasiquote b)))
	((quasiquote a) (quote a))
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

(define (list? x)
	(case-match x ()
		(() #t)
		((a . b) (list? b))
		(a #f)
	)
)

(define length (lambda (x)
	(if (null? x) 0 (+ (length (cdr x)) 1))
))

(define reverse (lambda (x)       
	(if (null? x) x (append (reverse (cdr x)) (list (car x))))
))

(define list-tail (lambda (x k)
	(if (zero? k) x (list-tail (cdr x) (- k 1)))
))

; non-standard (oddly)
(define list-head (lambda (x k)
	(if (zero? k) '() (cons (car x) (list-head (cdr x) (- k 1))))
))

(define list-ref (lambda (x k)
	(car (list-tail x k))
))

(define member (lambda (key lst)
	(if (null? lst) #f
		(if (equal? key (car lst)) lst
			(member key (cdr lst))
		)
	)
))

(define assoc (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k . v) . rest) (if (equal? k key) (cons k v) (assoc key rest)))
	)
))


(define (append . p)
	(case-match p ()
		(() '())
		((p) p)
		((p . rest) (letrec
			((append-two (lambda (a b)
				(case-match a ()
					(() b)
					((e . r) (cons e (append-two r b)))
				)
			)))
			(append-two p (apply append rest))
		))
	)
)

; 6.3.3 Symbols
(define symbol->string to-string)


; 6.3.4 Characters
(define char-numeric? (lambda (c)
	(if (char? c)
		(if (char-decimal-digit c) #t #f)
		#f
	)
))

(define char=? (lambda (a b)
	(= (char->integer a) (char->integer b))
))

(define char<? (lambda (a b)
	(< (char->integer a) (char->integer b))
))

(define char>? (lambda (a b)
	(> (char->integer a) (char->integer b))
))

(define char<=? (lambda (a b)
	(<= (char->integer a) (char->integer b))
))

(define char>=? (lambda (a b)
	(>= (char->integer a) (char->integer b))
))

(define char-ci=? (lambda (a b)
	(= (char->integer (char-upcase a)) (char->integer (char-upcase b)))
))

(define char-ci<? (lambda (a b)
	(< (char->integer (char-upcase a)) (char->integer (char-upcase b)))
))

(define char-ci>? (lambda (a b)
	(> (char->integer (char-upcase a)) (char->integer (char-upcase b)))
))

(define char-ci<=? (lambda (a b)
	(<= (char->integer (char-upcase a)) (char->integer (char-upcase b)))
))

(define char-ci>=? (lambda (a b)
	(>= (char->integer (char-upcase a)) (char->integer (char-upcase b)))
))

(define char-numeric? (lambda (c)
	(if (char-number c) #t #f)
))


; 6.3.5 Strings
(define string-copy (lambda (s)
	(string-append s)
))

(define string-same? (lambda (e sa sb)
	(define len (string-length sa))
	(and (= len (string-length sb))
		(let loop ((k 0))
			(or (= k len)
				(and
					(e (string-ref sa k) (string-ref sb k))
					(loop (+ k 1))
				)
			)
		)
	)
))

(define string-strict-order? (lambda (e f sa sb)
	(define la (string-length sa))
	(define lb (string-length sa))
	(let loop ((k 0))
		(and (not (= k lb))
			(or (= k la)
				(or (f (string-ref sa k) (string-ref sb k))
					(and (e (string-ref sa k) (string-ref sb k))
						(loop (+ k 1))
					)
				)
			)
		)
	)
))

(define string-inclusive-order? (lambda (e f sa sb)
	(define la (string-length sa))
	(define lb (string-length sa))
	(let loop ((k 0))
		(or (= k la)
			(and (not (= k lb))
				(or (f (string-ref sa k) (string-ref sb k))
					(and (e (string-ref sa k) (string-ref sb k))
						(loop (+ k 1))
					)
				)
			)
		)
	)
))

(define string=? (lambda (sa sb)
	(string-same? char=? sa sb)
))

(define string<? (lambda (sa sb)
	(string-strict-order? char=? char<? sa sb)
))

(define string>? (lambda (sa sb)
	(string-strict-order? char=? char>? sa sb)
))

(define string<=? (lambda (sa sb)
	(string-inclusive-order? char=? char<? sa sb)
))

(define string>=? (lambda (sa sb)
	(string-inclusive-order? char=? char>? sa sb)
))

(define string-ci=? (lambda (sa sb)
	(string-same? char-ci=? sa sb)
))

(define string-ci<? (lambda (sa sb)
	(string-strict-order? char-ci=? char-ci<? sa sb)
))

(define string-ci>? (lambda (sa sb)
	(string-strict-order? char-ci=? char-ci>? sa sb)
))

(define string-ci<=? (lambda (sa sb)
	(string-inclusive-order? char-ci=? char-ci<? sa sb)
))

(define string-ci>=? (lambda (sa sb)
	(string-inclusive-order? char-ci=? char-ci>? sa sb)
))

; Byte Arrays
(define byte-array-copy (lambda (s)
	(byte-array-append s)
))


; 6.4 Control Features
(define call-with-values (lambda (producer consumer)
	(apply consumer (values->list (producer)))
))

(define smap (lambda (proc a)
	(case-match a ()
		(() '())
		((ah . at) (cons (proc ah) (smap proc at)))
	)
))

(define map (lambda (proc a1 . ar)
	(case-match a1 ()
		(() '())
		((ah . at)
			(cons
				(apply proc (cons ah (smap car ar)))
				(apply map (cons proc (cons at (smap cdr ar))))
			)
		)
	)
))

(define for-each (lambda args
	(apply map args)
	<nothing>
))

(define-syntax catch (syntax-rules () ((catch foo catchclause)
	(call-with-current-continuation (lambda (cont)
		(let ((throw (lambda (ex) (cont (catchclause ex)))))
			foo
		)
	))
)))

;
;(define null-environment (let ((env (current-environment)))
;	(lambda () env)
;))
