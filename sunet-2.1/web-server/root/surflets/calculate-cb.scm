(define-structure surflet surflet-interface
  (open surflets
	surflets/callbacks		;make-callback
	surflet-requests
	handle-fatal-error
	let-opt
	scheme-with-scsh)
  (begin

    ;; This uses callbacks.
    
    (define *operator-alist*
	    `(("+" . ,+)
	      ("-" . ,-)
	      ("*" . ,*)
	      ("/" . ,/)))

    (define operator-symbol car)
    (define operator-operator cdr)

    (define operator-input-field
      (let ((name (generate-input-field-name "operator")))
	(make-input-field 
	 name
	 (lambda (input-field operator-string)
	   (cond 
	    ((assoc operator-string *operator-alist*) =>
	     (lambda (a) a))
	    (else
	     (error "no such operator" operator-string))))
	 `(select (@ (name ,name))
		  ,@(map (lambda (operator)
			   `(option ,(operator-symbol operator)))
			 *operator-alist*)))))

    (define (change-operator-callback)
      (make-callback 
       (lambda (req)
	 (change-operator
	  ;; This yields an error only when the browser is doing wrong.
	  (input-field-value operator-input-field
			     (get-bindings req))))
       ))


    (define (make-number-field/default default)
      (if default
	  (make-number-field `(@ (value ,default)))
	  (make-number-field)))
    
    (define (show-page operator-pair number1 number2 . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (number-field1 (make-number-field/default number1))
	     (number-field2 (make-number-field/default number2))
	     (req
	      (send-html/suspend
	       (lambda (new-url)
		 `(html 
		   (title "Simple calculator")
		   (body (h1 "Simple calculator")
			 (font (@ (color "red")) ,update-text)
			 (surflet-form
			  ,new-url
			  (table
			   (tr (td "Do calculation:"))
			   (tr (td ,number-field1) 
			       (td ,(operator-symbol operator-pair))
			       (td ,number-field2)
			       (td " = ")
			       (td ,(make-submit-button '(@ (value "calculate")))))))
			 (hr)
			 (p "You may choose another operator:")
			 (surflet-form
			  ,(change-operator-callback)
			  (table
			   (tr (td ,operator-input-field)
			       (td ,(make-submit-button 
				     '(@ (value "change operator"))))))))))))
	     (bindings (get-bindings req)))
	(let ((number1 (input-field-value number-field1 bindings))
	      (number2 (input-field-value number-field2 bindings)))
	  (if number1
	      (if number2
		  (calculate operator-pair number1 number2)
		  (show-page operator-pair number1 number2 "Please enter a valid second number."))
	      (show-page operator-pair number1 number2 "Please enter a valid first number."))
	  )))
		     
    (define (change-operator to-operation)
      (show-page to-operation #f #f))

    (define (calculate operator-pair number1 number2)
      (let ((operator (operator-operator operator-pair)))
	(show-result number1 (operator-symbol operator-pair) number2
		     (operator number1 number2))))

    (define (show-result number1 operator-symbol number2 result)
      (send-html
       `(html (title "Calculation Result")
	      (body (h1 "Result")
		    (p ,number1 " " ,operator-symbol " " ,number2
		       " = " ,result)))))

    (define (main req)
      (show-page (car *operator-alist*) #f #f)
      (error "This does not return"))
    ))