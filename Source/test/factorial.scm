(define fact (lambda (x)
  (if (equal? x 0) 1 (* x (fact (- x 1))))
))

(fact 1000)
