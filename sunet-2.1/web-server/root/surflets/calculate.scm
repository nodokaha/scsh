(define-structure surflet surflet-interface
  (open surflets
	surflets/my-input-fields
	surflet-requests
	handle-fatal-error
	let-opt
	scheme-with-scsh)
  (begin

    ;; This doesn't use c-a-l-l-b-a-c-k-s anymore.
    
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
	 name "operator"
	 (lambda (input-field operator-string)
	   (let ((operator (assoc operator-string *operator-alist*)))
	     (if operator
		 operator
		 (error "no such operator" operator-string))))
	 '()
	 (lambda (input-field)
	   `(select (@ (name ,name))
		    ,@(map (lambda (operator)
			     `(option ,(operator-symbol operator)))
			   *operator-alist*))))))


    (define (make-number-field/default default)
      (if default
	  (make-number-field default)
	  (make-number-field)))
    
    (define (show-page operator-pair number1 number2 . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (number-field1 (make-number-field/default number1))
	     (number-field2 (make-number-field/default number2))
	     (calculate-button (make-submit-button "Calculate"))
	     (change-button (make-submit-button "Change operator"))
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
			       (td ,calculate-button)))
			  (hr)
			  (p "You may choose another operator:")
			  (table
			   (tr (td ,operator-input-field)
			       (td ,change-button)))
			  (hr)
			  (p (url "/" "Return to main menu."))))))))
	     (bindings (get-bindings req)))
	(let ((number1 (input-field-value number-field1 bindings))
	      (number2 (input-field-value number-field2 bindings)))
	  (cond
	   ((returned-via? calculate-button bindings)
	    (if number1
		(if number2
		    (calculate operator-pair number1 number2)
		    (show-page operator-pair number1 number2 
			       "Please enter a valid second number."))
		(show-page operator-pair number1 number2 
			   "Please enter a valid first number.")))
	  ((returned-via? change-button bindings)
	   (with-fatal-error-handler
	    (lambda (c d)
	      ;; This should never happen.
	      (show-page operator-pair #f #f
			 "Internal error. Please retry or report."))
	    (show-page (input-field-value operator-input-field
					  bindings)
		       number1 number2)))
	  (else
	   ;; This should never happen.
	   (show-page operator-pair #f #f
		      "Internal error. Please retry or report."))))))
    
    (define (calculate operator-pair number1 number2)
      (let ((operator (operator-operator operator-pair)))
	(show-result number1 (operator-symbol operator-pair) number2
		     (operator number1 number2))))

    (define (show-result number1 operator-symbol number2 result)
      (send-html
       `(html (title "Calculation Result")
	      (body (h1 "Result")
		    (p ,number1 " " ,operator-symbol " " ,number2
		       " = " ,result)
		    (hr)
		    (p (url "calculate.scm" "Make new calculation") (br)
		       (url "/" "Return to main menu."))))))

    (define (main req)
      (show-page (car *operator-alist*) #f #f)
      (error "This does not return"))
    ))