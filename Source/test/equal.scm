"equal?"
(equal? 1 2)
(equal? 0 0)
(equal? 'a 'a)
(equal? 'a 'b)
(equal? 'a 0)
(equal? '(a b) '(a b))
(equal? '(a b) '(a 1))
(equal? '(a b) '())
(equal? '(a b) 4)
"Numbers"
(equal? #e2 #e2)
(equal? #i2 #i2)
(equal? #e2 #i2)
(equal? #e2 #e1)
"Procedures"
(equal? car cdr)
(equal? (lambda () '()) (lambda () '()))

"="
(= #e2 #e2)
(= #i2 #i2)
(= #e2 #i2)
(= #e2 #e1)
