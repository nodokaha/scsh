(define url-rule
  `(url *preorder*
	. ,(lambda (tag uri . maybe-text) 
	     (surflet-sxml->low-level-sxml
	      `(a (@ (href ,uri))
		  ,(if (null? maybe-text)
		       uri
		       maybe-text))))))

(define plain-html-rule
  `(plain-html
    *preorder*
    . ,(lambda (tag . text) text)))

(define nbsp-rule
  `(nbsp . ,(lambda (_) "&nbsp;")))

(define comment-rule
  `(*COMMENT* *preorder* 
              . ,(lambda (tag . elems)
                   `("<!-- " ,@elems "-->"))))

(define default-rules
 `(,attribute-rule
   ,default-rule
   ,text-rule
   ,comment-rule
   ,url-rule
   ,plain-html-rule
   ,nbsp-rule))

(define surflet-form-rule
  `(surflet-form 
    ;; Must do something to prevent the k-url string to be HTML
    ;; escaped.
    *preorder*
    . ,(lambda (trigger k-url . args)
	 (receive (parameters elems)
	     (optionals-first (list symbol? sxml-attribute?) args)
	   (make-surflet-form k-url ; k-url
			      (car parameters) ; POST, GET or #f=GET
			      (cadr parameters); attributes
			      elems)))))

(define (make-surflet-form k-url method attributes elems)
  (let ((real-method (case method
		       ((get GET) "GET")
		       ((post POST) "POST")
		       ((#f) "GET")
		       (else
			(error "invalid method type" method)))))
    (surflet-sxml->low-level-sxml 
     `(form (@ ((method ,real-method)
		(action ,k-url)
		,@(if attributes (cdr attributes) '())))
	    ;; cdr == sxml-attribute-attributes
	    ,@elems))))

(define input-field-rule
  `(,*input-field-trigger*
    *preorder*
    . ,(lambda input-field
	 (surflet-sxml->low-level-sxml
	  (input-field-html-tree input-field)))))

(define surflet-sxml-rules
  `(,@default-rules
     ;; form contents:
     ,input-field-rule
     ,surflet-form-rule))

;; Low-Level-SXML is a list that can be understood by
;; display-low-level-sxml. In contains only characters, strings, and
;; thunks.
(define (surflet-sxml->low-level-sxml sxml-tree)
  (sxml->low-level-sxml sxml-tree surflet-sxml-rules))



;;; Helping funtion for surflet-sxml-rule

;; PRED-LIST contains list of predicates that recognizes optional
;; leading parameters. FURTHER-ATTRIBUTES is the optional parameter
;; list as got by procedure call. TYPED-OPTIONALS returns two values:
;; a list of the same length as PRED-LIST and a list containing the
;; left arguments that did not fit the predicates.
;;
;; With the help of OPTIONALS-FIRST you can define a function
;; like `make-submit-button [string] [further-attributes]' this way:
;; (define (make-submit-button . args)
;;   (receive (params rest-args) 
;;     (prefix-optionals (list string? xml-attribute?) args)
;;     (if (pair? rest-args)
;;         (error "too many arguments to make-submit-button))
;;         (let ((value (first params))
;;               (attributes (second params)))
;;           ...))))
;;
(define (optionals-first pred-list args)
  (let loop ((results '())
	     (pred-list pred-list)
	     (args args))
    (cond
     ((null? pred-list)
      (values (reverse results) args))
     ((null? args)
      (values (rev-append results (make-list (length pred-list) #f)) '()))
     (((car pred-list) (car args))
      (loop (cons (car args) results)
	    (cdr pred-list)
	    (cdr args)))
     (else
      (loop (cons #f results)
	    (cdr pred-list)
	    args)))))
