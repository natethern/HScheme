(letrec
	(
		(a b)
		(b 7)
	)
	(list a b)
)

(letrec
	(
		(a 2)
		(b a)
	)
	(list a b)
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

(letrec
	(
		(e (string-append d "e"))
		(b (string-append a "b"))
		(a "a")
		(p (list a b c d e))
		(d (string-append c "d"))
		(c (string-append b "c"))
		(q (list e p))
	)
	(list a b c d e p q)
)
