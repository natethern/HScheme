"Loading init.full.scm"
(load "init.pure.scm")


; 4.1.6 Assignments
; Non-Standard
(define-syntax setf! (syntax-rules ()
	((setf! (part . a) b) ((concat "set-" part "!") a b))
	((setf! a b) (set! a b))
))


; 4.2.5 Delayed Evaluation
(define make-promise (lambda (proc)
	(let ((result-ready? #f) (result <nothing>))
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


; 6.4 Control Features
(define force (lambda (object)
	(object)
))


; 6.3.2 Pairs and Lists
(define memq (lambda (key lst)
	(if (null? lst) #f
		(if (eq? item (car lst)) lst
			(memq item (cdr lst))
		)
	)
))

(define memv (lambda (item lst)
	(if (null? lst) #f
		(if (eqv? item (car lst)) lst
			(memv item (cdr lst))
		)
	)
))

(define assq (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k v) . rest) (if (eq? k key) v (assq key rest)))
	)
))

(define assv (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k v) . rest) (if (eqv? k key) v (assv key rest)))
	)
))


; 6.6.1 Ports
(define call-with-input-file (lambda (name proc)
	(let ((port (open-input-file name)))
		(proc port)
		(close-input-port port)
	)
))

(define call-with-output-file (lambda (name proc)
	(let ((port (open-output-file name)))
		(proc port)
		(close-output-port port)
	)
))

; 6.6.2 Input
(define read (lambda rest
	(case-match rest ()
		(() (port-read (current-input-port)))
		((port) (port-read port))
	)
))

(define read-char (lambda rest
	(case-match rest ()
		(() (port-read-char (current-input-port)))
		((port) (port-read-char port))
	)
))

(define peek-char (lambda rest
	(case-match rest ()
		(() (port-peek-char (current-input-port)))
		((port) (port-peek-char port))
	)
))

(define char-ready? (lambda rest
	(case-match rest ()
		(() (port-char-ready? (current-input-port)))
		((port) (port-char-ready? port))
	)
))

; 6.6.3 Output
(define write-char (lambda (c . rest)
	(case-match rest ()
		(() (port-write-char c (current-input-port)))
		((port) (port-write-char c port))
	)
))

(define newline (lambda rest
	(apply write-char (cons #\newline rest))
))

"init.full.scm Loaded"
