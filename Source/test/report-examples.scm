"6.1  Equivalence predicates"

"procedure:  (eqv? obj1 obj2)"
(eqv? 'a 'a)							;===>  #t
(eqv? 'a 'b)							;===>  #f
(eqv? 2 2)								;===>  #t
(eqv? '() '())							;===>  #t
(eqv? 100000000 100000000)				;===>  #t
(eqv? (cons 1 2) (cons 1 2))			;===>  #f
(eqv? (lambda () 1) (lambda () 2))		;===>  #f
(eqv? #f 'nil)							;===>  #f
(let ((p (lambda (x) x))) (eqv? p p))	;===>  #t

(eqv? "" "")                     ;===>  unspecified
(eqv? '#() '#())                 ;===>  unspecified
(eqv? (lambda (x) x) (lambda (x) x))            ;===>  unspecified
(eqv? (lambda (x) x) (lambda (y) y))            ;===>  unspecified

(define gen-counter (lambda ()
	(let ((n 0))
		(lambda () (set! n (+ n 1)) n)
	)
))
(let ((g (gen-counter))) (eqv? g g))	;===>  #t
(eqv? (gen-counter) (gen-counter))		;===>  #f
(define gen-loser (lambda ()
	(let ((n 0))
		(lambda () (set! n (+ n 1)) 27)
	)
))
(let ((g (gen-loser))) (eqv? g g))		;===>  #t
(eqv? (gen-loser) (gen-loser))			;===>  unspecified

(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
         (g (lambda () (if (eqv? f g) 'both 'g))))
  (eqv? f g))
                                ;===>  unspecified

(letrec
	(
		(f (lambda () (if (eqv? f g) 'f 'both)))
		(g (lambda () (if (eqv? f g) 'g 'both)))
	)
	(eqv? f g)
)										;===>  #f

(eqv? '(a) '(a))                         ;===>  unspecified
(eqv? "a" "a")                           ;===>  unspecified
(eqv? '(b) (cdr '(a b)))                 ;===>  unspecified
(let ((x '(a)))  (eqv? x x))                            ;===>  #t

"procedure:  (eq? obj1 obj2)"
(eq? 'a 'a)                             ;===>  #t
(eq? '(a) '(a))                         ;===>  unspecified
(eq? (list 'a) (list 'a))               ;===>  #f
(eq? "a" "a")                           ;===>  unspecified
(eq? "" "")                             ;===>  unspecified
(eq? '() '())                           ;===>  #t
(eq? 2 2)                               ;===>  unspecified
(eq? #\A #\A)         ;===>  unspecified
(eq? car car)                           ;===>  #t
(let ((n (+ 2 3))) (eq? n n))			;===>  unspecified
(let ((x '(a))) (eq? x x))				;===>  #t
(let ((x '#())) (eq? x x))				;===>  #t
(let ((p (lambda (x) x))) (eq? p p))	;===>  #t

"library procedure:  (equal? obj1 obj2)"
(equal? 'a 'a)                          ;===>  #t
(equal? '(a) '(a))                      ;===>  #t
(equal? '(a (b) c)
        '(a (b) c))                     ;===>  #t
(equal? "abc" "abc")                    ;===>  #t
(equal? 2 2)                            ;===>  #t
(equal? (make-vector 5 'a)
        (make-vector 5 'a))             ;===>  #t
(equal? (lambda (x) x)
        (lambda (y) y))          ;===>  unspecified


"6.2.5  Numerical operations"

"procedure:  (number? obj)"
"procedure:  (complex? obj)"
"procedure:  (real? obj)"
"procedure:  (rational? obj)"
"procedure:  (integer? obj)"
(complex? 3+4i)                 ;===>  #t
(complex? 3)                    ;===>  #t
(real? 3)                       ;===>  #t
(real? -2.5+0.0i)               ;===>  #t
(real? #e1e10)                  ;===>  #t
(rational? 6/10)                ;===>  #t
(rational? 6/3)                 ;===>  #t
(integer? 3+0i)                 ;===>  #t
(integer? 3.0)                  ;===>  #t
(integer? 8/4)                  ;===>  #t

"library procedure:  (max x1 x2 ...)"
(max 3 4)                      ;===>  4    ; exact
(max 3.9 4)                    ;===>  4.0  ; inexact

"procedure:  (+ z1 ...)"
"procedure:  (* z1 ...)"
(+ 3 4)                         ;===>  7
(+ 3)                           ;===>  3
(+)                             ;===>  0
(* 4)                           ;===>  4
(*)                            ;===>  1

"procedure:  (- z1 z2)"
"procedure:  (- z)"
"optional procedure:  (- z1 z2 ...)"
"procedure:  (/ z1 z2)"
"procedure:  (/ z)"
"optional procedure:  (/ z1 z2 ...)"
(- 3 4)                         ;===>  -1
(- 3 4 5)                       ;===>  -6
(- 3)                           ;===>  -3
(/ 3 4 5)                       ;===>  3/20
(/ 3)                           ;===>  1/3

"library procedure:  (abs x)"
(abs -7)                        ;===>  7

"procedure:  (quotient n1 n2)"
"procedure:  (remainder n1 n2)"
"procedure:  (modulo n1 n2)"
(modulo 13 4)                   ;===>  1
(remainder 13 4)                ;===>  1
(modulo -13 4)                  ;===>  3
(remainder -13 4)               ;===>  -1
(modulo 13 -4)                  ;===>  -3
(remainder 13 -4)               ;===>  1
(modulo -13 -4)                 ;===>  -1
(remainder -13 -4)              ;===>  -1
(remainder -13 -4.0)            ;===>  -1.0  ; inexact

"library procedure:  (gcd n1 ...)"
"library procedure:  (lcm n1 ...)"
(gcd 32 -36)                    ;===>  4
(gcd)                           ;===>  0
(lcm 32 -36)                    ;===>  288
(lcm 32.0 -36)                  ;===>  288.0  ; inexact
(lcm)                           ;===>  1

"procedure:  (numerator q)"
"procedure:  (denominator q)"
(numerator (/ 6 4))          ;===>  3
(denominator (/ 6 4))          ;===>  2
(denominator
  (exact->inexact (/ 6 4)))         ;===> 2.0

"procedure:  (floor x)"
"procedure:  (ceiling x)"
"procedure:  (truncate x)"
"procedure:  (round x)"
(floor -4.3)                  ;===>  -5.0
(ceiling -4.3)                ;===>  -4.0
(truncate -4.3)               ;===>  -4.0
(round -4.3)                  ;===>  -4.0
(floor 3.5)                   ;===>  3.0
(ceiling 3.5)                 ;===>  4.0
(truncate 3.5)                ;===>  3.0
(round 3.5)                   ;===>  4.0  ; inexact
(round 7/2)                   ;===>  4    ; exact
(round 7)                     ;===>  7

"library procedure:  (rationalize x y)"
(rationalize
  (inexact->exact .3) 1/10)          ;===> 1/3    ; exact
(rationalize .3 1/10)                ;===> #i1/3  ; inexact


"6.2.6  Numerical input and output"

"procedure:  (string->number string radix)"
(string->number "100")                ;===>  100
(string->number "100" 16)             ;===>  256
(string->number "1e2")                ;===>  100.0
(string->number "15##")               ;===>  1500.0


"6.3.1  Booleans"
#t                 ;===>  #t
#f                ;===>  #f
'#f               ;===>  #f

"library procedure:  (not obj)"
(not #t)           ;===>  #f
(not 3)                  ;===>  #f
(not (list 3))           ;===>  #f
(not #f)          ;===>  #t
(not '())                ;===>  #f
(not (list))             ;===>  #f
(not 'nil)               ;===>  #f

"library procedure:  (boolean? obj)"
(boolean? #f)          ;===>  #t
(boolean? 0)                  ;===>  #f
(boolean? '())                ;===>  #f


"6.3.2  Pairs and lists"

(a b c d e)
(a . (b . (c . (d . (e . ())))))
(a b c . d)
(a . (b . (c . d)))

(define x (list 'a 'b 'c))
(define y x)
y                               ;===>  (a b c)
(list? y)                       ;===>  #t
(set-cdr! x 4)                  ;===>  unspecified
x                               ;===>  (a . 4)
(eqv? x y)                      ;===>  #t
y                               ;===>  (a . 4)
(list? y)                       ;===>  #f
(set-cdr! x x)                  ;===>  unspecified
(list? x)                       ;===>  #f

"procedure:  (pair? obj)"
(pair? '(a . b))                ;===>  #t
(pair? '(a b c))                ;===>  #t
(pair? '())                     ;===>  #f
(pair? '#(a b))                 ;===>  #f

"procedure:  (cons obj1 obj2)"
(cons 'a '())                   ;===>  (a)
(cons '(a) '(b c d))            ;===>  ((a) b c d)
(cons "a" '(b c))               ;===>  ("a" b c)
(cons 'a 3)                     ;===>  (a . 3)
(cons '(a b) 'c)                ;===>  ((a b) . c)

"procedure:  (car pair)" 
(car '(a b c))                  ;===>  a
(car '((a) b c d))              ;===>  (a)
(car '(1 . 2))                  ;===>  1
(car '())                       ;===>  error

"procedure:  (cdr pair)"
(cdr '((a) b c d))              ;===>  (b c d)
(cdr '(1 . 2))                  ;===>  2
(cdr '())                       ;===>  error

"procedure:  (set-car! pair obj)"
(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(set-car! (f) 3)                     ;===>  unspecified
(set-car! (g) 3)                     ;===>  error

"library procedure:  (list? obj)"
(list? '(a b c))             ;===>  #t
(list? '())                  ;===>  #t
(list? '(a . b))             ;===>  #f
(let ((x (list 'a)))
(set-cdr! x x)
(list? x))                 ;===>  #f

"library procedure:  (list obj ...)"
(list 'a (+ 3 4) 'c)                    ;===>  (a 7 c)
(list)                                  ;===>  ()

"library procedure:  (length list)"
(length '(a b c))                       ;===>  3
(length '(a (b) (c d e)))               ;===>  3
(length '())                            ;===>  0

"library procedure:  (append list ...)"
(append '(x) '(y))                      ;===>  (x y)
(append '(a) '(b c d))                  ;===>  (a b c d)
(append '(a (b)) '((c)))                ;===>  (a (b) (c))
(append '(a b) '(c . d))                ;===>  (a b c . d)
(append '() 'a)                         ;===>  a

"library procedure:  (reverse list)"
(reverse '(a b c))                      ;===>  (c b a)
(reverse '(a (b c) d (e (f))))			;===>  ((e (f)) d (b c) a)

"library procedure:  (list-ref list k)"
(list-ref '(a b c d) 2)                         ;===>  c
(list-ref '(a b c d) (inexact->exact (round 1.8)))	;===>  c

"library procedure:  (memq obj list)"
"library procedure:  (memv obj list)"
"library procedure:  (member obj list)"
(memq 'a '(a b c))                      ;===>  (a b c)
(memq 'b '(a b c))                      ;===>  (b c)
(memq 'a '(b c d))                      ;===>  #f
(memq (list 'a) '(b (a) c))             ;===>  #f
(member (list 'a) '(b (a) c))			;===>  ((a) c)
(memq 101 '(100 101 102))               ;===>  unspecified
(memv 101 '(100 101 102))               ;===>  (101 102)

"library procedure:  (assq obj alist)"
"library procedure:  (assv obj alist)"
"library procedure:  (assoc obj alist)"
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)             ;===>  (a 1)
(assq 'b e)             ;===>  (b 2)
(assq 'd e)             ;===>  #f
(assq (list 'a) '(((a)) ((b)) ((c))))	;===>  #f
(assoc (list 'a) '(((a)) ((b)) ((c))))	;===>  ((a))
(assq 5 '((2 3) (5 7) (11 13)))	;===>  unspecified
(assv 5 '((2 3) (5 7) (11 13)))	;===>  (5 7)



"6.3.3  Symbols"

"procedure:  (symbol? obj)"
(symbol? 'foo)                  ;===>  #t
(symbol? (car '(a b)))          ;===>  #t
(symbol? "bar")                 ;===>  #f
(symbol? 'nil)                  ;===>  #t
(symbol? '())                   ;===>  #f
(symbol? #f)             ;===>  #f

"procedure:  (symbol->string symbol)"
(symbol->string 'flying-fish)				;===>  "flying-fish"
(symbol->string 'Martin)					;===>  "martin"
(symbol->string (string->symbol "Malvina"))	;===>  "Malvina"

"procedure:  (string->symbol string)"
(eq? 'mISSISSIppi 'mississippi)				;===>  #t
(string->symbol "mISSISSIppi")				;===>  the symbol with name "mISSISSIppi"
(eq? 'bitBlt (string->symbol "bitBlt"))		;===>  #f
(eq? 'JollyWog (string->symbol  (symbol->string 'JollyWog)))	;===>  #t
(string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D.")))	;===>  #t


"6.3.4  Characters"

#\a
#\A
#\(
#\ 
#\space
#\newline

"procedure:  (string-set! string k char)"
(define (f) (make-string 3 #\*))
(define (g) "***")
(string-set! (f) 0 #\?)          ;===>  unspecified
(string-set! (g) 0 #\?)          ;===>  error
(string-set! (symbol->string 'immutable) 0 #\?)          ;===>  error


"6.3.6  Vectors"

'#(0 (2 2 2 2) "Anna")	;===>  #(0 (2 2 2 2) "Anna")

"library procedure:  (vector obj ...)"
(vector 'a 'b 'c)                       ;===>  #(a b c)

"procedure:  (vector-ref vector k)"
(vector-ref '#(1 1 2 3 5 8 13 21) 5)	;===>  8
(vector-ref '#(1 1 2 3 5 8 13 21)
	(let ((i (round (* 2 (acos -1)))))
		(if (inexact? i) (inexact->exact i) i)
	)
)										;===> 13

"procedure:  (vector-set! vector k obj)"
(let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec)	;===>  #(0 ("Sue" "Sue") "Anna")
(vector-set! '#(0 1 2) 1 "doe")			;===>  error  ; constant vector

"library procedure:  (vector->list vector)"
"library procedure:  (list->vector list)"
(vector->list '#(dah dah didah))		;===>  (dah dah didah)
(list->vector '(dididit dah))			;===>  #(dididit dah)


"6.4  Control features"

"procedure:  (procedure? obj)"
(procedure? car)                    ;===>  #t
(procedure? 'car)                   ;===>  #f
(procedure? (lambda (x) (* x x)))	;===>  #t
(procedure? '(lambda (x) (* x x)))	;===>  #f
(call-with-current-continuation procedure?)	;===>  #t

"procedure:  (apply proc arg1 ... args)"
(apply + (list 3 4))				;===>  7

(define compose (lambda (f g)
    (lambda args (f (apply g args)))
))
((compose sqrt *) 12 75)			;===>  30

"library procedure:  (map proc list1 list2 ...)"
(map cadr '((a b) (d e) (g h)))		;===>  (b e h)
(map (lambda (n) (expt n n)) '(1 2 3 4 5))	;===>  (1 4 27 256 3125)
(map + '(1 2 3) '(4 5 6))					;===>  (5 7 9)
(let ((count 0)) (map (lambda (ignored) (set! count (+ count 1))  count) '(a b)))	;===>  (1 2) or (2 1)

"library procedure:  (for-each proc list1 list2 ...)"
(let ((v (make-vector 5)))
	(for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4))
v)                                        ;===>  #(0 1 4 9 16)

"library procedure:  (force promise)"
(force (delay (+ 1 2)))           ;===>  3
(let ((p (delay (+ 1 2)))) (list (force p) (force p)))	;===>  (3 3)

(define a-stream
  (letrec ((next
            (lambda (n)
              (cons n (delay (next (+ n 1)))))))
    (next 0)))
(define head car)
(define tail
  (lambda (stream) (force (cdr stream))))

(head (tail (tail a-stream)))  
                                       ;===>  2

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
p                             ;===>  a promise
(force p)                     ;===>  6
p                             ;===>  a promise, still
(begin (set! x 10)
       (force p))             ;===>  6



(+ (delay (* 3 7)) 13)          ;===>  34

"procedure:  (call-with-current-continuation proc)"
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    #t))                                ;===>  -3

(define list-length (lambda (obj)
	(call-with-current-continuation (lambda (return)
		(letrec
			(
				(r (lambda (obj)
					(cond ((null? obj) 0)
						((pair? obj)
						(+ (r (cdr obj)) 1))
						(else (return #f))
					)
				))
			)
			(r obj)
		)
	))
))
(list-length '(1 2 3 4))                    ;===>  4
(list-length '(a b . c))                    ;===>  #f

"procedure:  (call-with-values producer consumer)"
(call-with-values (lambda () (values 4 5))
                  (lambda (a b) b))
                                                           ;===>  5

(call-with-values * -)                                     ;===>  -1


"procedure:  (dynamic-wind before thunk after)"

(let ((path '()) (c #f))
	(let ((add (lambda (s) (set! path (cons s path)))))
		(dynamic-wind
			(lambda () (add 'connect))
			(lambda ()
				(add (call-with-current-continuation
					(lambda (c0) (set! c c0) 'talk1)
				))
			)
			(lambda () (add 'disconnect))
		)
		(if (< (length path) 4)
			(c 'talk2)
			(reverse path)
		)
	)
)															;===> (connect talk1 disconnect connect talk2 disconnect)


"6.5  Eval"

"procedure:  (eval expression environment-specifier)"
(eval '(* 7 3) (scheme-report-environment 5))				;===>  21

(let
	((f
		(eval '(lambda (f x) (f x x)) (null-environment 5))
	))
	(f + 10)
)															;===>  20
