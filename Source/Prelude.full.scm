"Loading Prelude.full.scm"
(load "Prelude.pure.scm")

; 6.4 Control Features
(define force (lambda (object)
	(object)
))

(define make-promise (lambda (proc)
	(let ( (result-ready? #f) (result <nothing>))
		(lambda ()
			(if result-ready? result
				(let ((x (proc)))
					(if result-ready? result
						(begin
							(set! result-ready? #t)
							(set! result x)
							result
						)
					)
				)
			)
		)
	)
))

(define delay (syntax-rules ()
	((delay expression) (make-promise (lambda () expression)))
))

;
(define-syntax setf! (syntax-rules ()
	((setf! (part . a) b) ((concat "set-" part "!") a b))
	((setf! a b) (set! a b))
))

"Prelude.full.scm Loaded"
