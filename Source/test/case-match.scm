(case-match '(hello) ()
	(() p)
	((a) (list a 'q))
)

(define get-type (lambda (x)
	(case-match x (q)
		(() 'empty)
		((h . t) 'pair)
		(q 'q)
		(a 'other)
	)
))

(define subst-other (lambda (x)
	(case-match x (bb dd)
		(() '())
		((h . t) (cons (subst-other h) (subst-other t)))
		(bb 'b)
		(dd 'd)
		(a 'x)
	)
))

"get-type"
(get-type 'pqr)
(get-type 'q)
(get-type '())
(get-type '(a (b c) d))
(get-type '(p q r . s))

"subst-other"
(subst-other 'aa)
(subst-other '(aa))
(subst-other 'bb)
(subst-other '(bb))
(subst-other '())
(subst-other '(aa (bb cc) dd))
(subst-other '(p q r . s))
