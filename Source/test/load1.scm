(define a1 'a1)
(define-syntax s1 (syntax-rules ()
	((s1 a) (list 's1 a))
))

(s1 (list 'p1 'q1 a1))
