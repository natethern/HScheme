(let ((x 'outer))
	(let-syntax ((m (syntax-rules () ((m y) (list x y)))))
		(let ((x 'inner))
			(m x)
		)
	)
)

(let-syntax ((m (syntax-rules () ((m y) (let ((x 'outer)) y)))))
	(let ((x 'inner))
		(m x)
	)
)

(let-syntax ((m (syntax-rules () ((m y) (list y 'y)))))
	(m (cons 'a 'b))
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-1))))
	(let-syntax
		(
			(sa (syntax-rules () ((sa) 'inner-1)))
			(sb (syntax-rules () ((sb) (sa))))
		)
		(list (sa) (sb))
	)
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-2))))
	(letrec-syntax
		(
			(sa (syntax-rules () ((sa) 'inner-2)))
			(sb (syntax-rules () ((sb) (sa))))
		)
		(list (sa) (sb))
	)
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-3))))
	(let-syntax
		(
			(sa (syntax-rules () ((sa) 'inner-3)))
			(sb (syntax-rules () ((sb x) (list (sa) (x)))))
		)
		(sb sa)
	)
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-4))))
	(letrec-syntax
		(
			(sa (syntax-rules () ((sa) 'inner-4)))
			(sb (syntax-rules () ((sb x) (list (sa) (x)))))
		)
		(sb sa)
	)
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-5))))
	(let-syntax
		(
			(sb (syntax-rules () ((sb) (sa))))
			(sa (syntax-rules () ((sa) 'inner-5)))
		)
		(list (sa) (sb))
	)
)

(let-syntax ((sa (syntax-rules () ((sa) 'outer-6))))
	(letrec-syntax
		(
			(sb (syntax-rules () ((sb) (sa))))
			(sa (syntax-rules () ((sa) 'inner-6)))
		)
		(list (sa) (sb))
	)
)
