(letrec
	(
		(a 3)
		(b 4)
	)
	(list a b)
)

(letrec
	((fact
		(lambda (x)
			(if (equal? x 0) 1 (* x (fact (- x 1))))
		)
	))
	(fact 40)
)

(letrec
	(
		(fact (lambda (x)
			(if (equal? x 0) 1 (fmult x))
		))
		
		(fmult (lambda (x)
			(* x (fact (- x 1)))
		))
	)
	(fact 42)
)
