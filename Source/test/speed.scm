(define starttime (current-time))

(define namespace (lambda (ns) (lambda (x) (string-append ns x))))
(define rdf (namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(define rdfs (namespace "http://www.w3.org/2000/01/rdf-schema#"))

(define type-p (rdf "type"))
(define label-p (rdfs "label"))
(define comment-p (rdfs "comment"))

(define-syntax ifnull (syntax-rules ()
  ((ifnull f x alt) (let ((xx x)) (if (null? xx) alt (f xx))))
))

(define (rdf-select temp asserts)
	'()
)

(define name (lambda (id)
 (ifnull caar (rdf-select `((string-literal label)) `((,id ,label-p label)))
  (ifnull caar (rdf-select `((uri ,id)) `())
   (ifnull caar (rdf-select `((string-literal ,id)) `())
    (ifnull
     (lambda (cid) (string-append "some " (name (caar cid))))
     (rdf-select `(class) `((,id ,type-p class)))
     "something"
))))))

(define res-name (lambda (id)
 (ifnull caar (rdf-select `((string-literal label)) `((,id ,label-p label)))
  (ifnull caar (rdf-select `((uri ,id)) `())
   (ifnull
    (lambda (cid) (string-append "some " (name (caar cid))))
    (rdf-select `(class) `((,id ,type-p class)))
    "something"
)))))

(define xml:escape-char (lambda (c)
  (case c 
   ((#\<) "&lt;")
   ((#\>) "&gt;")
   ((#\&) "&amp;")
   ((#\") "&quot;")
   (else (string c))
  )
))

(define xml:escape (lambda (text)
	text
;  (apply string-append (map xml:escape-char (string-chars text)))
))

(define xml:attrs-text (lambda (attrs)
  (define attr-text (lambda ((aname aval))
    (string-append " " aname "=\"" (xml:escape aval) "\"")
  ))
  (apply string-append (map attr-text attrs))
))

(define xml:full-tag (lambda (ttype attrs text)
  (string-append "<" ttype (xml:attrs-text attrs) ">" (xml:escape text) "</" ttype ">")
))

(define xml:tag (lambda (ttype attrs text)
  (if (equal? text "")
   (string-append "<" ttype (xml:attrs-text attrs) "/>")
   (xml:full-tag ttype attrs text)
  )
))

(define xml:content-tag (lambda (ttype attrs text)
  (if (equal? text "") "" (xml:full-tag ttype attrs text))
))

(define html:link (lambda (link text)
  (xml:content-tag "A" `(("HREF" ,link)) text)
;  (string-append link text)
))

(define linked (lambda (id text)
  (html:link (string-append "ladle.cgi?id=" (to-string id)) text)
))

(define linked-name (lambda (id)
  (linked id (name id))
))

(define linked-res-name (lambda (id)
  (linked id (res-name id))
))


(define (numlist x) (if (= x 0) '() (cons x (numlist (- x 1)))))
(- (current-time) starttime)

(define nl (numlist 141))
(- (current-time) starttime)

(define result
(map
 (lambda (id)
;  `("<LI>" ,(linked id "hello") " (" ,id ")" #\newline)
  (string-append "<LI>" (linked id "hello") " (" (to-string id) ")" (string #\newline))
 )
 (numlist 141)
)
)
(- (current-time) starttime)
