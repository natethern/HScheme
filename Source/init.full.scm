(load "init.pure.scm")


; 4.1.6 Assignments
; Non-Standard
(define-syntax setf! (syntax-rules ()
	((setf! (part . a) b) ((concat "set-" part "!") a b))
	((setf! a b) (set! a b))
))


; 4.2.5 Delayed Evaluation
(define make-promise (lambda (proc)
	(let ((result-ready? #f) (result #f))
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

(define-syntax delay (syntax-rules ()
	((delay expression) (make-promise (lambda () expression)))
))


; 6.4 Control Features
(define force (lambda (object)
	(object)
))


; 6.3.2 Pairs and Lists
(define memq (lambda (key lst)
	(if (null? lst) #f
		(if (eq? key (car lst)) lst
			(memq key (cdr lst))
		)
	)
))

(define memv (lambda (key lst)
	(if (null? lst) #f
		(if (eqv? key (car lst)) lst
			(memv key (cdr lst))
		)
	)
))

(define assq (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k . v) . rest) (if (eq? k key) (cons k v) (assq key rest)))
	)
))

(define assv (lambda (key alist)
	(case-match alist ()
		(() #f)
		(((k . v) . rest) (if (eqv? k key) (cons k v) (assv key rest)))
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

(define read-byte (lambda rest
	(port-read-byte (optional-input-port rest))
))

(define peek-byte (lambda rest
	(port-peek-byte (optional-input-port rest))
))

(define byte-ready? (lambda rest
	(port-byte-ready? (optional-input-port rest))
))

(define read-char-latin1 (lambda rest
	(port-read-char-latin1 (optional-input-port rest))
))

(define read-char-utf8 (lambda rest
	(port-read-char-utf8 (optional-input-port rest))
))

(define peek-char-latin1 (lambda rest
	(port-peek-char-latin1 (optional-input-port rest))
))

(define read-latin1 (lambda rest
	(port-read-latin1 (optional-input-port rest))
))

(define read-utf8 (lambda rest
	(port-read-utf8 (optional-input-port rest))
))

(define char-ready-latin1? byte-ready?)


(define read-char read-char-latin1)

(define peek-char peek-char-latin1)

(define char-ready? char-ready-latin1?)

(define read read-latin1)


; 6.6.3 Output
(define optional-output-port (lambda (rest)
	(case-match rest ()
		(() (current-output-port))
		((port) port)
	)
))

(define write-byte (lambda (b . rest)
	(port-write-byte b (optional-output-port rest))
))

(define write-byte-array (lambda (s . rest)
	(port-write-byte-array s (optional-output-port rest))
))

(define write-char-latin1 (lambda (c . rest)
	(port-write-char-latin1 c (optional-output-port rest))
))

(define write-char-utf8 (lambda (c . rest)
	(port-write-char-utf8 c (optional-output-port rest))
))

(define write-string-latin1 (lambda (s . rest)
	(port-write-string-latin1 s (optional-output-port rest))
))

(define write-string-utf8 (lambda (s . rest)
	(port-write-string-utf8 s (optional-output-port rest))
))

(define write-latin1 (lambda (s . rest)
	(apply write-string-latin1 (cons (to-string s) rest))
))

(define write-utf8 (lambda (s . rest)
	(apply write-string-utf8 (cons (to-string s) rest))
))

(define display-latin1 (lambda (s . rest)
	(apply write-string-latin1 (cons (to-display s) rest))
))

(define display-utf8 (lambda (s . rest)
	(apply write-string-utf8 (cons (to-display s) rest))
))

(define newline (lambda rest
	(apply write-char-latin1 (cons #\newline rest))
))

(define write-char write-char-latin1)

(define write-string write-string-latin1)

(define write write-latin1)

(define display display-latin1)
