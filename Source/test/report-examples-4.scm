"Chapter 4"
"4.1  Primitive expression types"
"4.1.1  Variable references"
(define x 28)
x							;===>  28

"4.1.2  Literal expressions"
(quote a)					;===>  a
(quote #(a b c))			;===>  #(a b c)
(quote (+ 1 2))				;===>  (+ 1 2)
'a							;===>  a
'#(a b c)					;===>  #(a b c)
'()							;===>  ()
'(+ 1 2)					;===>  (+ 1 2)
'(quote a)					;===>  (quote a)
''a							;===>  (quote a)
'"abc"						;===>  "abc"
"abc"						;===>  "abc"
'145932						;===>  145932
145932						;===>  145932
'#t							;===>  #t
#t							;===>  #t

"4.1.3  Procedure calls"
(+ 3 4)						;===>  7
((if #f + *) 3 4)			;===>  12

"4.1.4  Procedures"
(lambda (x) (+ x x))		;===>  a procedure
((lambda (x) (+ x x)) 4)	;===>  8
(define reverse-subtract
	(lambda (x y) (- y x))
)
(reverse-subtract 7 10)		;===>  3
(define add4
	(let ((x 4))
		(lambda (y) (+ x y))
))
(add4 6)					;===>  10
((lambda x x) 3 4 5 6)		;===>  (3 4 5 6)
((lambda (x y . z) z)
	3 4 5 6
)							;===>  (5 6)

"4.1.5  Conditionals"
(if (> 3 2) 'yes 'no)		;===>  yes
(if (> 2 3) 'yes 'no)		;===>  no
(if (> 3 2)
	(- 3 2)
	(+ 3 2)
)							;===>  1

"4.1.6  Assignments"
(define x 2)
(+ x 1)						;===>  3
(set! x 4)					;===>  unspecified
(+ x 1)						;===>  5


"4.2  Derived expression types"
"4.2.1  Conditionals"
(cond
	((> 3 2) 'greater)
	((< 3 2) 'less)
)							;===>  greater
(cond
	((> 3 3) 'greater)
	((< 3 3) 'less)
	(else 'equal)
)							;===>  equal
(cond
	((assv 'b '((a 1) (b 2))) => cadr)
	(else #f)
)							;===>  2
(case (* 2 3)
	((2 3 5 7) 'prime)
	((1 4 6 8 9) 'composite)
)							;===>  composite
(case (car '(c d))
	((a) 'a)
	((b) 'b)
)							;===>  unspecified
(case (car '(c d))
	((a e i o u) 'vowel)
	((w y) 'semivowel)
	(else 'consonant)
)							;===>  consonant
(and (= 2 2) (> 2 1))		;===>  #t
(and (= 2 2) (< 2 1))		;===>  #f
(and 1 2 'c '(f g))			;===>  (f g)
(and)						;===>  #t
(or (= 2 2) (> 2 1))		;===>  #t
(or (= 2 2) (< 2 1))		;===>  #t
(or #f #f #f)				;===>  #f
(or (memq 'b '(a b c)) 
	(/ 3 0))				;===>  (b c)

"4.2.2  Binding constructs"
(let ((x 2) (y 3))
	(* x y)
)							;===>  6
(let ((x 2) (y 3))
(let
		(
			(x 7)
			(z (+ x y))
		)
		(* z x)
	)
)							;===>  35
(let ((x 2) (y 3))
	(let*
		(
			(x 7)
			(z (+ x y))
		)
		(* z x)
	)
)							;===>  70
(letrec
	(
		(even? (lambda (n)
				(if (zero? n) #t (odd? (- n 1)))
		))
		(odd? (lambda (n)
				(if (zero? n) #f (even? (- n 1)))
		))
	)
	(even? 88)
)							;===>  #t

"4.2.3  Sequencing"
(define x 0)
(begin
	(set! x 5)
	(+ x 1)
)							;===>  6
(begin
	(display "4 plus 1 equals ")
	(display (+ 4 1))
)							;===>  unspecified
;  and prints  4 plus 1 equals 5
(newline)

"4.2.4  Iteration"
(do
	(
		(vec (make-vector 5))
		(i 0 (+ i 1))
	)
	((= i 5) vec)
	(vector-set! vec i i)
)							;===>  #(0 1 2 3 4) 

(let ((x '(1 3 5 7 9)))
	(do
		(
			(x x (cdr x))
			(sum 0 (+ sum (car x)))
		)
		((null? x) sum)
	)
)							;===>  25

(let loop
	(
		(numbers '(3 -2 1 6 -5))
		(nonneg '())
		(neg '())
	)
	(cond
		((null? numbers) (list nonneg neg))
		((>= (car numbers) 0)
			(loop (cdr numbers)
			(cons (car numbers) nonneg)
			neg)
		)
		((< (car numbers) 0)
			(loop (cdr numbers) nonneg (cons (car numbers) neg))
		)
	)
)							;===>  ((6 1 3) (-5 -2))

"4.2.6  Quasiquotation"
`(list ,(+ 1 2) 4)			;===>  (list 3 4)
(let ((name 'a))
	`(list ,name ',name)
)							;===>  (list a (quote a))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)           
							;===>  (a 3 4 5 6 b)
`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))           
							;===>  ((foo 7) . cons)
`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)           
							;===>  #(10 5 2 4 3 8)
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)           
							;===>  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
(let ((name1 'x) (name2 'y))
	`(a `(b ,,name1 ,',name2 d) e)
)							;===>  (a `(b ,x ,'y d) e)
(quasiquote (list (unquote (+ 1 2)) 4))           
							;===>  (list 3 4)
'(quasiquote (list (unquote (+ 1 2)) 4))           
							;===>  `(list ,(+ 1 2) 4)
							; i.e., (quasiquote (list (unquote (+ 1 2)) 4))


"4.3  Macros"
"4.3.1  Binding constructs for syntactic keywords"
(let-syntax
	((when
		(syntax-rules ()
			((when test stmt1 stmt2 ...)
				(if test
					(begin stmt1
						stmt2 ...
					)
				)
			)
		)
	))
	(let ((if #t))
		(when if (set! if 'now))
		if
	)
)							;===>  now

(let ((x 'outer))
	(let-syntax ((m (syntax-rules () ((m) x))))
		(let ((x 'inner))
			(m)
		)
	)
)							;===>  outer

;(letrec-syntax
;	((my-or
;		(syntax-rules ()
;			((my-or) #f)
;			((my-or e) e)
;			((my-or e1 e2 ...)
;				(let ((temp e1))
;					(if temp temp
;						(my-or e2 ...)
;					)
;				)
;			)
;		)
;	))
;	(let ((x #f) (y 7) (temp 8) (let odd?) (if even?))
;		(my-or x
;			(let temp)
;			(if y)
;			y
;		)
;	)
;)							;===>  7


"4.3.2  Pattern language"
;(let ((=> #f))
;	(cond (#t => 'ok))
;)							;===> ok
