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

(define hello-goodbye-etc (lambda (x) (cons 'hello (cons 'goodbye x)) ))

(hello-goodbye-etc '(a b c))

(define hello-goodbyes1 (letrec
	((h (hello-goodbye-etc h)))
	h
))

(define hello-goodbyes2 (call-with-result hello-goodbye-etc))

(define list-head (lambda (x k)
	(if (zero? k) '() (cons (car x) (list-head (cdr x) (- k 1))))
))

(list-head hello-goodbyes1 30)
(list-head hello-goodbyes2 30)
