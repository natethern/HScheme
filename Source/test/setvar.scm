(let
	((q 67))
	(begin-list
		(port-write-byte 65 (current-output-port))
		(to-string (set! q 68))
		(port-write-byte 66 (current-output-port))
		(port-write-byte q (current-output-port))
		(to-string q)
	)
)


(define a 3)
;(define b a)
;(define bf (lambda () a))
(set! a 5)
a
a
;b
;(bf)
