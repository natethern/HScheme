(let
	((a 45))
	a
)

(let
	((a 32))
	(let (
		(a 17)
		(b (+ a 4))
		)
		(list a b)
	)
)

(let
	((a 32))
	(let* (
		(a 17)
		(b (+ a 4))
		)
		(list a b)
	)
)
