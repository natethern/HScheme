(define v '#(3 4 5))
(vector? v)
v
(vector? '(a b c))
(define p5v (make-vector 5 'p))
p5v
(vector-length p5v)
(vector-ref v 1)
(vector->list v)
(list->vector '(p q r))
