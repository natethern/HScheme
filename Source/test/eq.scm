"eq?"
(eq? #e2 #e2)
(eq? #i2 #i2)
(eq? #e2 #i2)
(eq? #e2 #e1)
(eq? car car)
(eq? car cdr)
(let ((f (lambda () '())))
	(eq? f f)
)
(eq? (lambda () '()) (lambda () '()))

"eqv?"
(eqv? #e2 #e2)
(eqv? #i2 #i2)
(eqv? #e2 #i2)
(eqv? #e2 #e1)
(eqv? car car)
(eqv? car cdr)
(let ((f (lambda () '())))
	(eqv? f f)
)
(eqv? (lambda () '()) (lambda () '()))

"equal?"
(equal? #e2 #e2)
(equal? #i2 #i2)
(equal? #e2 #i2)
(equal? #e2 #e1)
(equal? car car)
(equal? car cdr)
(let ((f (lambda () '())))
	(equal? f f)
)
(equal? (lambda () '()) (lambda () '()))

"="
(= #e2 #e2)
(= #i2 #i2)
(= #e2 #i2)
(= #e2 #e1)

"list?"
(define list-head (lambda (x k)
	(if (zero? k) '() (cons (car x) (list-head (cdr x) (- k 1))))
))

(define as (call-with-result (lambda (tail) (cons 'a tail))))
(define abs (call-with-result (lambda (tail) (cons 'a (cons 'b tail)))))

(list-head as 30)

(list? '())
(list? '(p))
(list? '(p q))
(list? '(p q . r))
(list? '(p q r r r r r))
(list? as)
(list? (cons 'p as))
(list? abs)
(list? (cons 'p abs))
