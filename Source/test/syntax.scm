(define-syntax simple (syntax-rules ()
	((simple) 12)
	((simple x) x)
))

(simple)
(simple 5)

(define-syntax withliterals (syntax-rules (alit blit)
	((withliterals alit blit . x) (cons 'ab 'x))
	((withliterals blit) 'wlblit)
	((withliterals . x) 'x)
))

(define alit 'a)
(define blit 'b)

(withliterals alit)
(withliterals blit)
(withliterals alit blit 3)
(withliterals blit alit 4)


(define-syntax ell (syntax-rules ()
	((ell (p (q r)) ...) '((p ...) (q ...) (r ...)))
))

(ell)
(ell (a (b c)) (d (e f)) (37 ("hello" ())) ((a b) ((c . d) (e f g))))
