(define-syntax simple (syntax-rules ()
	((simple) 12)
	((simple x) x)
))

(simple)
(simple 5)
