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
		(pq '(p q))
		(rpq (cons 'r pq))
	)
	(list pq rpq)
)
