(define display (lambda (s . rest)
	(apply write-string-latin1 (cons (to-display s) rest))
))

(define write-string-latin1 (lambda (s)
	(port-write-string-latin1 s (current-output-port))
))

"start 1"
(let ((cont #f))
	(letrec
		(
			(x (call-with-current-continuation (lambda (c) (set! cont c) 0)))
	   		(y (call-with-current-continuation (lambda (c) (set! cont c) 0)))
	   	)
		(if cont
      		(let ((c cont))
				(set! cont #f)
				(set! x 1)
				(set! y 1)
				(c 0)
			)
			(+ x y)
		)
	)
)
"done 1"

"start 2"
(display "A\n")

(define cont #f)

(letrec
	(
		(x 
			(begin
				(call-with-current-continuation (lambda (c)
(display "D\n")
					(set! cont c)
(display "E\n")
					0
				))
(display "F\n")
				0
			)
		)
	)
	(if cont
		(let ((c cont))
(display "B\n")
			(set! cont #f)
(display "C\n")
			(c 0)
		)
		55
	)
)
"done 2"
