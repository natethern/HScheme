
(define a2 'a2)
(define-syntax s2 (syntax-rules ()
	((s2 a) (list 's2 a))
))

(s2 (list 'p2 'q2 a2))
