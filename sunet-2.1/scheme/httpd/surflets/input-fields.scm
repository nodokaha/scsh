;;; Copyright 2002, 2003 Andreas Bernauer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input-fields
;;; defines input-fields for surflets

;;; Globals
(define *input-field-trigger* `*input-field*)
(define generate-input-field-name generate-unique-name)

;;; Data structure for real-input-field
;; MULTI?: Transformer will get all bindings of request, not only the
;; one concerning the input-field.
(define-record-type real-input-field :real-input-field
  (make-real-input-field name type transformer
			 attributes html-tree-maker
			 html-tree multi?)
  real-input-field?
  (name real-input-field-name)
  (type real-input-field-type)
  (transformer real-input-field-transformer)
  (attributes real-input-field-attributes set-real-input-field-attributes!)
  (html-tree-maker real-input-field-html-tree-maker)
  (html-tree real-input-field-html-tree set-real-input-field-html-tree!)
  (multi? real-input-field-multi?))

(define-record-discloser :real-input-field
  (lambda (input-field)
    (list 'real-input-field
	  (real-input-field-type input-field)
	  (real-input-field-name input-field))))

;;; Fake input-field record. This is necessary, as the trigger in SXML
;;; may only be symbols, not arbitrary values. Thus, our input-fields
;;; must be preceeded by a trigger symbol to get recognized by the
;;; SXML transforming routines like sxml->html.

;; Constructors: make-input-field, make-multi-input-field

;; Predicates: input-field?

;; Selectors: input-field-name, input-field-type,
;; input-field-transformer, input-field-attributes,
;; input-field-html-tree-maker, input-field-html-tree,
;; input-field-multi?

;; Mutators: set-input-field-attributes!, touch-input-field!

;;; Constructors for input-field / multi-input-field
(define (make-input-field name type transformer attributes 
			  html-tree-maker)
  (make-sxml-input-field
   (make-real-input-field name type transformer 
			  attributes html-tree-maker #f #f)))

(define (make-multi-input-field name type transformer attributes 
				html-tree-maker)
  (make-sxml-input-field
   (make-real-input-field name type transformer 
			  attributes html-tree-maker #f #t)))

(define (make-sxml-input-field real-input-field)
  (list *input-field-trigger* real-input-field))

(define input-field-real-input-field cadr)

(define (input-field? input-field)
  (and (pair? input-field)
       (eq? *input-field-trigger* (car input-field))
       (real-input-field? (input-field-real-input-field input-field))))


(define (make-input-field-selector selector)
  (lambda (input-field)
    (selector (input-field-real-input-field input-field))))

(define (make-input-field-setter setter reset?)
  (lambda (input-field value)
    (let ((real-input-field (input-field-real-input-field input-field)))
      (if reset?
	  (set-real-input-field-html-tree! real-input-field #f))
      (setter real-input-field value))))

(define input-field-name (make-input-field-selector real-input-field-name))
(define input-field-type (make-input-field-selector real-input-field-type))
(define input-field-transformer 
  (make-input-field-selector real-input-field-transformer))
(define input-field-attributes 
  (make-input-field-selector real-input-field-attributes))
(define input-field-html-tree-maker
  (make-input-field-selector real-input-field-html-tree-maker))
(define (input-field-html-tree input-field)
  (let ((real-input-field (input-field-real-input-field input-field)))
    (cond
     ((real-input-field-html-tree real-input-field)
      => identity)
     (else 
      (let ((html-tree ((real-input-field-html-tree-maker real-input-field)
			input-field)))
	(set-real-input-field-html-tree! real-input-field html-tree)
	html-tree)))))

(define input-field-multi?
  (make-input-field-selector real-input-field-multi?))

(define set-input-field-attributes! 
  (make-input-field-setter set-real-input-field-attributes! #t))
;; not exported:
(define set-input-field-html-tree! 
  (make-input-field-setter set-real-input-field-html-tree! #f))

;; A touched input-field's html-tree will be recalculated if
;; neccessary.
(define (touch-input-field! input-field)
  (set-input-field-html-tree! input-field #f))

;; <input-field>: '(input-field <real-input-field>)
;; <real-input-field>: #{Real-input-field "name"}
(define (raw-input-field-value input-field bindings)
  (let ((real-input-field (input-field-real-input-field input-field)))
    (cond
     ((real-input-field-multi? real-input-field)
      ((real-input-field-transformer real-input-field) input-field bindings))
     ((real-input-field-binding real-input-field bindings) =>
      (lambda (binding)
	((real-input-field-transformer real-input-field) 
	 input-field (cdr binding))))
     (else
      (error "no such input-field" input-field bindings)))))

;; Trys to get a value for INPUT-FIELD in BINDINGS. If this fails
;; (i.e. RAW-INPUT-FIELD-VALUE returns an error), the default-value is
;; returned. The default-value defaults to #f. NOTE: If you do this
;; with input-fields whose valid values may be the same as the default
;; value, you cannot determine by the result if there was such a value
;; or not. Keep in mind, that RAW-INPUT-FIELD-VALUE returns also an
;; error, if there was not such an input field. This makes
;; INPUT-FIELD-VALUE working with checkbox input fields because they
;; miss if they are not checked.
(define (input-field-value input-field bindings . maybe-default)
  (let ((default (:optional maybe-default #f)))
    (with-fatal-error-handler
     (lambda (condition more)
;       (format #t "hit error condition: ~s~%" condition)
       default)
     (raw-input-field-value input-field bindings))))

(define (real-input-field-binding input-field bindings)
  (assoc (real-input-field-name input-field) bindings))

;; Returns the binding of the input-field in bindings by the
;; input-field's name. If your input-field will have another name in
;; the bindings than it was created with, use a multi-input-field.
(define (input-field-binding input-field bindings)
  (real-input-field-binding (input-field-real-input-field input-field)
			    bindings))


;;EOF