;;; Simple code for doing structured html output. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; Copyright (c) 1996 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; - An attribute-quoter, that will map an attribute value to its
;;;   HTML text representation -- surrounding it with single or double quotes,
;;;   as appropriate, etc.

;;; Printing HTML tags.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All the emit-foo procedures have the same basic calling conventions:
;;;     (emit-foo out <required values> ... [<extra attributes> ...])
;;; - OUT is either a port or #t for the current input port.
;;; - Each attribute is either a (name . value) pair, which is printed as
;;;      name="value"
;;;   or a single symbol or string, which is simply printed as-is
;;;   (this is useful for attributes that don't have values, such as the
;;;   ISMAP attribute in <img> tags).



;;;     <tag name1="val1" name2="val2" ...>

(define (emit-tag out tag . attrs)
  (let ((out (fmt->port out)))
    (display "<" out)
    (display tag out)
    (for-each (lambda (attr)
		(display #\space out)
		(cond ((pair? attr)			; name="val"
		       (display (car attr) out)
		       (display "=\"" out)		; Should check for
		       (display (cdr attr) out)		; internal double-quote
		       (display #\" out))		; etc.
		      (else
		       (display attr out))))		; name
	      attrs)
    (display #\> out)))


;;; </tag>

(define (emit-close-tag out tag)
  (format out "</~a>" tag))


;;; <P>

(define (emit-p . args)		; (emit-p [out attr1 ...])
  (receive (out attrs) (if (pair? args)
			   (let* ((out (car args)))
			     (values (if (eq? out #t) (current-output-port) out)
				     (cdr args)))
			   (values (current-output-port) args))

    (apply emit-tag out 'p attrs)))


;;; <TITLE> Make Money Fast!!! </TITLE>

(define (emit-title out title)			; Takes no attributes.
  (format out "<title>~a~%</title>~%" title))

(define (emit-header out level text . attribs)
  (apply with-tag* out (string-append "H" (number->string level))
	 (lambda () (display text (fmt->port out)))
	 attribs))
	     
;;; ...and so forth. Could stand to define a bunch of little emitters for the
;;; various tags. (define-tag-emitter ...)


;;; Printing out balanced <tag> ... </tag> pairs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (with-tag out tag (attr-elt ...) body ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Execute the body forms between a <tag attrs> ... </tag> pair.
;;; The (ATTR-ELT ...) list specifies the attributes for the <tag>.
;;; It is rather like a LET-list, having the form
;;;     ((name val) ...)
;;; Each NAME must be a symbol, and each VAL must be a Scheme expression 
;;; whose value is the string to use as attribute NAME's value. Attributes
;;; that have no value (e.g., ISMAP) can be specified as attr-elt NAME, 
;;; instead of (NAME VALUE).
;;;
;;; For example,
;;;     (let ((hp "http://clark.lcs.mit.edu/~shivers")) ; My home page.
;;;       (with-tag port A ((href hp-url) (name "hp"))
;;;         (display "home page" port)))
;;; outputs
;;;     <A href="http://clark.lcs.mit.edu/~shivers" name="hp">home page</A>

(define-syntax with-tag
  (syntax-rules ()
    ((with-tag out tag (attr-elt ...) body ...)
     (with-tag* out 'tag (lambda () body ...)
		(%hack-attr-elt attr-elt)
		...))))

;;; Why does this have to be top-level? 
;;; Why can't this be a LET-SYNTAX inside of WITH-TAG?

(define-syntax %hack-attr-elt 
  (syntax-rules ()			; Build attribute-list element:
    ((%hack-attr-elt (name val))	; (name elt) => (cons 'name elt)
     (cons 'name val))
    ((%hack-attr-elt name) 'name)))	; name => 'name


;;; Execute THUNK between a <tag attrs> ... </tag> pair.

(define (with-tag* out tag thunk . attrs)
  (apply emit-tag out tag attrs)
  (let ((out (fmt->port out)))
    (call-with-values thunk
		      (lambda results
			(emit-close-tag out tag)
			(apply values results)))))


(define (fmt->port x)
  (if (eq? x #t) (current-output-port) x))

;;; Translate text to HTML, mapping special chars such as <, >, &, and
;;; double-quote to their HTML escape sequences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note iso8859-1 above 127 is perfectly OK

(define *html-entity-alist*
  (list
   (cons (ascii->char 60) "&lt;")
   (cons (ascii->char 62) "&gt;")
   (cons (ascii->char 38) "&amp;")
   (cons (ascii->char 34) "&quot;")))

(define *html-entities*
  (list->char-set (map car *html-entity-alist*)))

(define *html-entity-table*
  (let ((v (make-vector 256 #f)))
    (for-each (lambda (entry)
		(vector-set! v
			     (char->ascii (car entry))
			     (cdr entry)))
	      *html-entity-alist*)
    v))

(define (string-set-substring! t start s)
  (let* ((l (string-length s))
	 (end (+ l start)))
    (do ((i start (+ 1 i)))
	((= i end) t)
      (string-set! t i (string-ref s (- i start))))))

(define (escape-html s)
  (let ((target-length
	 (string-fold (lambda (c i)
			(+ i
			   (if (char-set-contains? *html-entities* c)
			       (string-length 
				(vector-ref *html-entity-table*
					    (char->ascii c)))
			       1)))
		      0
		      s)))
    (if (= target-length (string-length s))
	s
	(let ((target (make-string target-length)))
	  (string-fold
	   (lambda (c i)
	     (+ i
		(if (char-set-contains? *html-entities* c)
		    (let ((entity (vector-ref *html-entity-table* (char->ascii c))))
		      (string-set-substring! target i entity)
		      (string-length entity))
		    (begin 
		      (string-set! target i c)
		      1))))
	   0
	   s)
	  target))))

(define (emit-text s . maybe-port)
  (if (null? maybe-port)
      (write-string (escape-html s))
      (write-string (escape-html s) (fmt->port (car maybe-port)))))
