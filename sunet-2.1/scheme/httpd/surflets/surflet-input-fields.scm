;;; SUrflets' input fields
;;; Copyright 2002, 2003 Andreas Bernauer
;;; With additions from Eric Knauel (2003)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to create simple input fields

;; The interface for input-fields does not prescribe what the type of
;; attributes has to be. We choose a record here.

(define-record-type field-attributes :field-attributes
  (make-field-attributes default others)
  field-attributes?
  (default field-attributes-default set-field-attributes-default!)
  (others field-attributes-others set-field-attributes-others!))

;; A simple input-field is a prototype for other input-fields.
;; REPORTED-TYPE is the type of the input-field in HTML, TYPE the
;; internal referenced type and TRANSFORMER the function that
;; translates the HTTP-string of the request into a scheme value.
(define (simple-field-maker reported-type type default-pred transformer)
  (lambda maybe-further-attributes
    (let ((name (generate-input-field-name type)))
      (let-optionals maybe-further-attributes
	  ((default "" default-pred)
	   (attributes '() sxml-attribute?))
	(make-input-field name type
			  transformer
			  (make-field-attributes
			   (and default `(value ,default))
			   (sxml-attribute-attributes attributes))
			  (simple-html-tree-maker reported-type))))))

(define (simple-html-tree-maker reported-type)
  (lambda (input-field)
    (let ((attributes (input-field-attributes input-field)))
      `(input (@ (type ,reported-type)
		 (name ,(input-field-name input-field))
		 ,(field-attributes-default attributes)
		 ,(field-attributes-others attributes))))))

(define (make-simple-default-setter default-pred? error-msg-types)
  (lambda (input-field value)
    (if (default-pred? value)
	(set-field-attributes-default! 
	 (input-field-attributes input-field) 
	 `(value ,value))
	(error (format #f "Default value must be ~a." error-msg-types) 
	       value))
    (touch-input-field! input-field)))

(define (string-or-symbol? thing) 
  (or (string? thing) (symbol? thing)))
(define simple-default? string-or-symbol?)

(define set-simple-field-default! 
  (make-simple-default-setter simple-default? "a string or a symbol"))

(define (second-arg first second) second)

;;;;;;;;;;;;;;;;;;;;
;;; Text input field
(define make-text-field 
  (simple-field-maker "text" "text" simple-default? second-arg))
(define set-text-field-value! set-simple-field-default!)

;;;;;;;;;;;;;;;;;;;;;;
;;; Number input field
(define (number-field-default? value)
  (or (number? value)
      (simple-default? value)))
(define (number-field-transformer input-field string)
  (or (string->number string)
      (error "wrong type")))
(define make-number-field
  (simple-field-maker "text" "number" 
		      number-field-default? number-field-transformer))
(define set-number-field-value!
  (make-simple-default-setter number-field-default? 
			      "a number a string or a symbol"))

;;;;;;;;;;;;;;;;;;;;;;
;;; hidden input-field
;; The programmer should supply a default value for this input-field
;; as it is hidden.
(define make-hidden-field
  (simple-field-maker "hidden" "hidden" 
		      simple-default? second-arg))
(define set-hidden-field-value! set-simple-field-default!)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Password input field
(define make-password-field 
  (simple-field-maker "password" "password" 
		      simple-default? second-arg))
(define set-password-field-value! set-simple-field-default!)

;;; That's it for simple input fields.

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Textarea input field
(define (make-textarea .  maybe-further-attributes)
  (let-optionals maybe-further-attributes
      ((default-text "" simple-default?)
       (rows 5 number?)
       (cols 20 number?)
       (readonly #f boolean?)
       (attributes '() sxml-attribute?))
    (let ((name (generate-input-field-name "textarea"))
	  (all-attributes `((cols ,cols)
			    (rows ,rows)
			    ,@(if readonly '(readonly) '())
			    ,@(sxml-attribute-attributes attributes))))
      (make-input-field 
       name "textarea"
       second-arg
       (make-field-attributes (and default-text)
			      all-attributes)
       make-textarea-html-tree))))

(define (make-textarea-html-tree textarea)
  (let ((attributes (input-field-attributes textarea)))
    `(textarea (@ (type "textarea")
		  (name ,(input-field-name textarea))
		  ,(field-attributes-others attributes))
	       ,(field-attributes-default attributes))))

(define (set-textarea-value! textarea value)
    (if (simple-default? value)
	(set-field-attributes-default! 
	 (input-field-attributes textarea)
 	 value)
	(error "Default value must be a string or a symbol." value))
    (touch-input-field! textarea))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Select input field

;(make-select '("this" "that" "those") '(@ ((id "sushi"))))
;(make-select '("this" ("that" '(@ (selected))) "those"))
;; dropdown: (size 1)

;;; A select input field shows a list of options that can be
;;; selected. For this purpose, we introduce an select-option record,
;;; that contains all the information for each option. This is
;;; justified by the fact, that the options list is seperated in HTML,
;;; too. The TAG is the string that is displayed in the website, the
;;; VALUE is the value that is returned by input-field-value, if this
;;; option was selected. TAG is assumed to be unique by some functions
;;; (e.g. select and unselect) SELECTED? tells us, if this option is
;;; preselected.
(define-record-type select-option :select-option
  (really-make-select-option tag value selected? attributes)
  select-option?
  (tag select-option-tag)
  (value select-option-value)
  (selected? select-option-selected? really-set-select-option-selected?!)
  (attributes select-option-attributes set-select-option-attributes!))

;; No check of attributes as this is done by calling function. (This
;; function isn't exported.
(define (make-select-option tag value selected? attributes)
  (if (string? tag)
      (really-make-select-option tag value selected?
				 (sxml-attribute-attributes attributes))
      (error "Select-option's tag must be a string." tag)))

;; Constructor for valued select input-field option.
(define (make-annotated-select-option tag value . maybe-attributes)
  (let-optionals maybe-attributes
      ((selected? #f boolean?)
       (attributes '() sxml-attribute?))
    (make-select-option tag value selected? attributes)))

;; Constructor for a simple select input-field option (not annotated).
(define (make-simple-select-option tag . maybe-attributes)
  (let-optionals maybe-attributes
      ((selected? #f boolean?)
       (attributes '() sxml-attribute?))
    (make-select-option tag tag selected? attributes)))

(define-record-discloser :select-option
  (lambda (select-option)
    (list 'select-option
	  (select-option-tag select-option)
	  (select-option-value select-option)
	  (select-option-selected? select-option)
	  (select-option-attributes select-option)
	  )))

;; Selecting / Unselecting of an option in an select input-field,
;; chosen by tag.
(define (select-select-option! tag select)
  (set-select-option-selected?! tag select #t))

(define (unselect-select-option! tag select)
  (set-select-option-selected?! tag select #f))

(define (set-select-option-selected?! tag select selected?)
  (let ((options (field-attributes-default 
		  (input-field-attributes select))))
    (if (number? tag)			; is tag an index?
	(really-set-select-option-selected?! (list-ref options tag) 
					     selected?)
	(let lp ((options options))
	  (if (null? options)
	      (error "No such option" tag select)
	      (if (tag=select-option? tag (car options))
		  (really-set-select-option-selected?! (car options) 
						       selected?)
		  (lp (cdr options))))))
    (touch-input-field! select)))

;; Find select-option in a list by its tag.
(define (tag=select-option? tag select-option)
  (string=? tag (select-option-tag select-option)))

(define (find-select-option tag select-options)
  (cond ((member/srfi-1 tag select-options tag=select-option?)
	 => car)			
	;; MEMBER/SRFI-1 returns the sublist that starts with the
	;; searched element.
	(else #f)))

(define (find-select-option-value tag select-options)
  (cond ((find-select-option tag select-options)
	 => select-option-value)
	(else #f)))

(define (add-select-option! select select-option)
  (let ((attributes (input-field-attributes select)))
    (set-field-attributes-default!
     attributes
     (cons select-option
	   (field-attributes-default attributes)))
    (touch-input-field! select)))

(define (delete-select-option! select select-option)
  (let* ((attributes (input-field-attributes select))
	 (select-options (field-attributes-default attributes)))
    (if (select-option? select-option)
	(set-field-attributes-default!
	 attributes
	 (delete select-option select-options))
	(let ((tag select-option))
	  (set-field-attributes-default!
	   attributes
	   (delete tag select-options tag=select-option?))))
    (touch-input-field! select)))

;; To be compatible with previous versions of MAKE-SELECT-INPUT-FIELD,
;; we accept also a simple list as an option-list. New programs should
;; use select-options-list (easily createable with 
;; (map make-simple-select-option option-list))
(define (simple-options select-options)
  (if (and (list? select-options)
	   (every select-option? select-options))
      select-options
      (map make-simple-select-option select-options)))

(define (make-select select-options . maybe-further-attributes)
  (let ((real-select-options (simple-options select-options)))
    (let-optionals maybe-further-attributes
	((multiple? #f boolean?)
	 (attributes '() sxml-attribute?))
      (let ((name (generate-input-field-name "select")))
	(if multiple?
	    (make-multiple-select name real-select-options attributes)
	    (make-single-select name real-select-options 
				attributes))))))

;; deprecated: Does not introduce further functionality.
(define make-annotated-select make-select)

;; internal
(define (make-multiple-select name select-options attributes)
  (make-multi-input-field name "mult-select"
			  multiple-select-transformer
			  (make-field-attributes
			   select-options 
			   `((multiple) 
			     ,@(sxml-attribute-attributes attributes)))
			  make-select-html-tree))

;; internal
(define (make-single-select name select-options attributes)
  (make-input-field name "select" 
		    (lambda (input-field tag)
		      (cond ((find-select-option-value tag select-options)
			     => identity)
			    (else (error "no such option." tag))))
		    (make-field-attributes
		     select-options 
		     (sxml-attribute-attributes attributes))
		    make-select-html-tree))

(define (multiple-select-transformer select bindings)
  (let ((name (input-field-name select))
	(select-options (field-attributes-default 
			 (input-field-attributes select))))
    (let* ((my-bindings (filter (lambda (binding)
				  (equal? (car binding) name))
				bindings))
	   (tags (map cdr my-bindings)))
      (filter-map (lambda (tag)
		    (find-select-option-value tag select-options))
		  tags))))

(define (make-select-html-tree select)
  (let ((attributes (input-field-attributes select)))
    `(select (@ (name ,(input-field-name select))
		,(field-attributes-others attributes))
	     #\newline
	     ,@(make-select-options-html-tree 
		(field-attributes-default attributes)))))

(define (make-select-options-html-tree select-options)
  (map (lambda (select-option)
	 `(option (@ ,(and (select-option-selected? select-option) '(selected))
		     ,(select-option-attributes select-option))
		  ,(select-option-tag select-option)))
       select-options))

;;;;;;;;;;;;;;;;;;;;;;
;;; radio input-fields	    
;; Because grouped radio input-fields must use the same name, we
;; cannot just return one radio input-field object, but we must
;; generate several ones with the same name.
(define (make-radio-group)
  (let ((name (generate-input-field-name "radio")))
    (lambda (value-string . maybe-further-attributes)
      (let-optionals maybe-further-attributes
	  ((checked? #f boolean?)
	   (attributes '() sxml-attribute?))
	(make-input-field name "radio"
			  second-arg
			  (make-field-attributes 
			   (and checked? '(checked))
			   `((value ,value-string)
			     ,@(sxml-attribute-attributes attributes)))
			  radio-html-tree-maker)))))

(define (make-annotated-radio-group)
  (let* ((name (generate-input-field-name "radio"))
	 (value-table (make-integer-table))
	 (transformer (make-radio-transformer value-table)))
    (lambda (value . maybe-further-attributes)
      (let-optionals maybe-further-attributes
	  ((checked? #f boolean?)
	   (attributes '() sxml-attribute?))
	(let ((number (generate-unique-number)))
	  (table-set! value-table number value)
	  (make-input-field name "radio"
			    transformer
			    (make-field-attributes 
			     (and checked? '(checked))
			     `((value ,(number->string number))
			       ,@(sxml-attribute-attributes attributes)))
			    radio-html-tree-maker))))))

(define (make-radios values . maybe-further-attributes)
  (let-optionals maybe-further-attributes
      ((attributes '() sxml-attribute?))
    (let ((radio-gen (make-annotated-radio-group)))
      (map (lambda (value)
	     (if attributes
		 (radio-gen value attributes)
		 (radio-gen value)))
	   values))))


(define (make-radio-transformer value-table)
  (lambda (input-field tag)
    (cond 
     ((string->number tag) =>
      (lambda (number)
	(let ((value (table-ref value-table number)))
	  (if value
	      value
	      (error "Unknown tag number for radio input-field" tag)))))
     (else
      (error "Unknown tag number for radio input-field" tag)))))

(define (radio-html-tree-maker radio)
  (let* ((attributes (input-field-attributes radio)))
    `(input (@ ((type "radio")
		(name ,(input-field-name radio))
		,(field-attributes-default attributes)
		,(field-attributes-others attributes))))))

(define (set-input-field-checked?! input-field checked?)
  (let ((attributes (input-field-attributes input-field)))
    (set-field-attributes-default! 
     attributes
     (if checked? '(checked) #f))
    (touch-input-field! input-field)))

(define set-radio-checked?! set-input-field-checked?!)
(define (check-radio! radio) (set-radio-checked?! radio #t))
(define (uncheck-radio! radio) (set-radio-checked?! radio #f))
;;;;;;;;;;;;;;;;;;;;;;;;
;;; checkbox input-field
(define (make-checkbox . maybe-further-attributes)
  (really-make-checkbox 'defined-in-checkbox-transformer
			checkbox-transformer
			maybe-further-attributes))

(define (make-annotated-checkbox value . maybe-further-attributes)
  (really-make-checkbox value 
			(make-checkbox-transformer value)
			maybe-further-attributes))

(define (really-make-checkbox value transformer attributes)
  (let ((name (generate-input-field-name "checkbox")))
    (let-optionals attributes
	((checked? #f boolean?)
	 (attributes '() sxml-attribute?))
      (make-input-field name "checkbox"
			transformer
			(make-field-attributes
			 (and checked? '(checked))
			 (sxml-attribute-attributes attributes))
			checkbox-html-tree-maker))))

(define (make-checkbox-transformer value)
  (lambda (input-field tag)
    (if (string=? tag "on")
	value
	#f)))

(define checkbox-transformer (make-checkbox-transformer #t))

(define (checkbox-html-tree-maker checkbox)
  (let ((attributes (input-field-attributes checkbox)))
    `(input (@ ((type "checkbox")
		(name ,(input-field-name checkbox))
		,(field-attributes-default attributes)
		,(field-attributes-others attributes))))))

(define set-checkbox-checked?! set-input-field-checked?!)
(define (check-checkbox! checkbox) (set-checkbox-checked?! checkbox #t))
(define (uncheck-checkbox! checkbox) (set-checkbox-checked?! checkbox #f))


;;;;;;;;;;;;;;;;;;;;;;
;; button input-fields
(define (make-button type name button-caption attributes)
  (make-input-field name type
		    second-arg
		    (make-field-attributes
		     (and button-caption `(value ,button-caption))
		     (sxml-attribute-attributes attributes))
		    make-button-html-tree))

(define (make-button-html-tree button)
  (let ((attributes (input-field-attributes button)))
    `(input (@ (type ,(input-field-type button))
	       (name ,(input-field-name button))
	       ,(field-attributes-default attributes)
	       ,(field-attributes-others attributes)))))

(define (make-submit-button . maybe-further-attributes)
  (let-optionals maybe-further-attributes
      ((button-caption #f string?)
       (attributes '() sxml-attribute?))
    (make-button "submit" (generate-input-field-name "submit")
		 button-caption attributes)))

(define (make-reset-button . maybe-further-attributes)
  (let-optionals maybe-further-attributes
      ((button-caption #f string?)
       (attributes '() sxml-attribute?))
    (make-button "reset" (generate-input-field-name "reset")  
		 button-caption attributes)))

;; Image buttons cannot be simple buttons, as the browser does not
;; send their simple name, but the coordinates where the user clicked
;; into. Thanks to Eric Knauel for reporting this bug.
(define (make-image-button image-source . maybe-further-attributes)
  (let-optionals maybe-further-attributes
      ((attributes '() sxml-attribute?))
    (make-multi-input-field (generate-input-field-name "imgbtn")
			    "image"
			    image-button-transformer
			    (make-field-attributes
			     `(src ,image-source) 
			     (sxml-attribute-attributes attributes))
			    make-button-html-tree)))

;; The following two functions are from Eric Knauel's fix for the
;; image-button bug:
(define (image-button-transformer image-button bindings)
  (let ((x (find-image-button-coordinate image-button bindings ".x"))
	(y (find-image-button-coordinate image-button bindings ".y")))
    (let ((x-number (string->number x))
	  (y-number (string->number y))) 
      (and x y
	   (if (and x-number y-number)
	       (cons x-number y-number)
	       (error "Image button coordinates aren't numbers. " x y))))))

(define (find-image-button-coordinate image-button bindings suffix)
  (let* ((name (input-field-name image-button)))
    (cond 
     ((assoc (string-append name suffix) bindings)
      => cdr)
     (else #f))))

;;EOF
