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
(define optional-input-port (lambda (rest)
	(case-match rest ()
		(() (current-input-port))
		((port) port)
	)
))

(define read (lambda rest
	(port-read (optional-input-port rest))
))

(define read-byte (lambda rest
	(port-read-byte (optional-input-port rest))
))

(define read-char-latin1 (lambda rest
	(port-read-char-latin1  (optional-input-port rest))
))

(define read-char-utf8 (lambda rest
	(port-read-char-utf8  (optional-input-port rest))
))

(define peek-char-latin1 (lambda rest
	(port-peek-char-latin1  (optional-input-port rest))
))

(define read-char read-char-latin1)

(define peek-char peek-char-latin1)

(define char-ready? (lambda rest
	(port-char-ready?  (optional-input-port rest))
))


; 6.6.3 Output
(define optional-output-port (lambda (rest)
	(case-match rest ()
		(() (current-output-port))
		((port) port)
	)
))

(define write-char (lambda (c . rest)
	(port-write-char c  (optional-output-port rest))
))

(define newline (lambda rest
	(apply write-char (cons #\newline rest))
))

"init.full.scm Loaded"
