(define-syntax and (syntax-rules ()
	((and) #t)
	((and first . rest) (if first (and . rest) #f))
))

(and #t #t #t)
(and #t #t #f)
(and #t #t)
(and #t #f)
(and #f #t)
(and #f #f)
(and #t)
(and #f)
(and)