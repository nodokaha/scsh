;;; Simple Surflet API, shamelessly adapted / copied from PLT.
;;; Copyright 2002, Andreas Bernauer

;; Send a query, suspend the current program, and produce for an
;; answer that matches query.
(define (single-query query)
  (car (get-results (list query) "Web Query")))

(define (queries queries)
  (get-results queries "Web Query"))

(define (form-query assoc-query-list)
  (zip (map car assoc-query-list)
       (get-results (map cadr assoc-query-list) "Web Query")))

(define (get-results queries title . maybe-update-text+defaults)
  (let-optionals maybe-update-text+defaults
      ((update-text #f)
       (defaults (make-list (length queries) #f)))
    (let* ((queries (map transform-string-to-query queries))
	   (req (send-html/suspend
		 (lambda (new-url)
		   (generate-simple-surflet-page new-url update-text
						 title
						 queries defaults))))
	   (bindings (get-bindings req))
	   (queries+values (map (lambda (query)
				  (cons query (ask query 'value bindings)))
				queries))
	   (bad-query+value (find (lambda (query+value) 
				    (not (cdr query+value)))
				  queries+values)))
      (if bad-query+value
	  (get-results queries title
		       (ask (car bad-query+value) 'bad-input-text)
		       (map (lambda (query+value)
			      (let ((value (cdr query+value)))
				(and value
				     (value-value value))))
			    queries+values))
	  (map (lambda (query+value) 
		 (value-value (cdr query+value)))
	       queries+values)))))

(define (generate-simple-surflet-page new-url update-text title queries defaults)
  `(html 
    (title ,title)
    (body (@ (bgcolor "white"))
	  (h3 ,(if update-text 
		   `(font (@ (color "red")) ,update-text) 
		   title))
	  (surflet-form ,new-url POST
			(table ,@(map (lambda (query default)
					(ask query 'html-table-row default))
				      queries defaults))
			,(make-submit-button)))))

;; Post some information on a Web page, wait for continue signal.
(define (inform title . text)
  (send-html/suspend 
   (lambda (url)
     `(html 
       (title ,title)
       (body (@ (bgcolor "white"))
	     (h3 ,title)
	     (br)
	     (p ,@text)
	     (br)
	     (url ,url "Continue"))))))

;; Post some information on a Web page, shut down the surflet and all
;; its continuations.
(define (final-page title . text)
  (send-html/finish
   `(html 
     (title ,title)
     (body (@ (bgcolor "white"))
	   (h3 ,title)
	   (br)
	   (p ,@text)))))


;(define-record-type query :query
;  (make-query type text input-field insist)
;  query?
;  (type query-type)
;  (text query-text)
;  (input-field query-input-field)
;  (insist query-insist))

(define-record-type value :value
  (make-value value)
  value?
  (value value-value))

(define (standard-query text input-field insist)
  (lambda (message)
    (case message
      ((html-table-row)
       (lambda (self default)
	 ;; DEFAULT is ignored currently. There is a problem with
	 ;; adding the default-value to an already generated
	 ;; input-field.
	 `(tr (td ,text) (td ,input-field))))
      ((value)
       (lambda (self bindings)
	 ;; Return #f, if getting value failed, otherwise a value
	 ;; object containing the value. This lets the checkbox field
	 ;; to return #f as a valid value.
	 (with-fatal-error-handler
	  (lambda (c m) #f)	
	  (make-value (raw-input-field-value input-field bindings)))))
      ((bad-input-text)
       (lambda (self)
	 (format #f "~a to the question: ~a" insist text)))
      (else (no-method message)))))

(define (make-text text)
  (standard-query text (make-text-field) "No bad input possible"))

(define (make-password text)
  (standard-query text (make-password-field) "No bad input possible"))

(define (make-number text) 
  (standard-query text (make-number-field) "Please respond with a valid number"))

(define (make-boolean text)
  (let* ((input-field (make-checkbox))
	 (standard (standard-query text input-field "No bad input possible")))
    (lambda (message)
      (case message
       ((value)
	(lambda (self bindings)
	  (if (input-field-binding input-field bindings) 
	      (make-value #t)
	      (make-value #f))))
       (else
	(get-method standard message))))))

(define (make-radio text choices . maybe-insist)
  (let* ((insist (:optional maybe-insist ""))
	 (radios (make-radios choices))
	 (standard (standard-query text (car radios) 
				   (string-append "Please respond" insist))))
    (lambda (message)
      (case message
	((html-table-row)
	 (lambda (self default)
	   ;; See note above for default.
	   `(tr (td ,text)
		(td (table (tr
			    ,@(map (lambda (radio choice)
				     `((td ,radio ,choice 
					   ;; Add some distance
					   (nbsp)(nbsp))))
				   radios
				   choices)))))))
	(else
	 (get-method standard message))))))

(define (make-yes-no text yes-text no-text)
  (make-radio text 
	      (list yes-text no-text)
	      (format #f " with ~s or ~s" yes-text no-text)))

(define (transform-string-to-query query)
  (if (string? query)
      (make-text query)
      query))

(define (extract/single symbol table)
  (let ((entries (extract symbol table)))
    (if (= 1 (length entries))
	(car entries)
	(error "Symbol occurs zero times or more than once." symbol table))))

(define (extract symbol table)
  (map cadr
       (filter (lambda (entry) (equal? symbol (car entry)))
	       table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OOP
;;; from Mike Sperber (lecture winter term 1999/2000)
;; Objects are procedures returning methods

(define get-method
  (lambda (object message)
    (object message)))

;; The return value of NO-METHOD must be distinguishable from methods.

(define-record-type no-method :no-method
  (no-method name)
  no-method?
  (name no-method-name))

(define (method? x)
  (not (no-method? x)))

;; ASK gets a method and calls it
(define (ask object message . args)
  (let ((method (get-method object message)))
    (if (no-method? method)
	(error "No method" message (no-method-name method))
	(apply method (cons object args)))))
