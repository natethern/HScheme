(letrec
	((fact
		(lambda (x)
			(if (equal? x 0) 1 (* x (fact (- x 1))))
		)
	))
	(fact 20)
)

(letrec
	(
		(e d)
		(b a)
		(a 37)
		(d c)
		(c b)
	)
	e
)
