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
