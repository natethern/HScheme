(let
	((q 4))
	(body-list
		(set! q 7)
		q
	)
)

(define a 3)
(define b a)
(define bf (lambda () a))
(set! a 5)
a
a
b
(bf)
