; top-level form of begin
(let ()
	(define a 3)
	(define b (+ a 1))
	(define f (lambda () (+ g 2)))
	(begin
		(define c (+ b 3))
		(define g 37)
		(define h (f))
	)
	(list a b f c g h (f))
)

; macro form of begin
(define p (begin
	(define f (lambda () (+ x 4)))
	(define x 51)
	(list f (f) x)
))
p
