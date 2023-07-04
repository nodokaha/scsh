(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin

    (define number-input (make-number-field))

    (define (create-input-page title input-text number-input)
      (lambda (new-url)
	`(html (title ,title)
	       (body
		(h2 ,title)
		(p
		 (surflet-form ,new-url
			       ,input-text " "
			       ,number-input
			       ,(make-submit-button)))
		(hr)
		(p (url "/" "Return to main menu.") (br)
		   (url "add-surflet.scm" "Start new calculation."))))))

    (define (get-number title input-text)
      (let* ((result (send-html/suspend
		      (create-input-page title input-text number-input)))
	     (bindings (get-bindings result))
	     (number (input-field-value number-input bindings)))
	(if number
	    number
	    (get-number title "Please enter a valid number."))))

    (define (get-number-1)
      (get-number "Addition - Step one" "First number:"))

    (define (get-number-2)
      (get-number "Addition - Step two" "Second number:"))

    (define (main req)
      (let ((number-1 (get-number-1))
            (number-2 (get-number-2)))
        (show-result (+ number-1 number-2))))
    
    (define (show-result result)
      (send-html 
       `(html (title "Result")
              (body (h2 "Result")
                    (p ,result
                       (hr)
                       (p (a (@ (href "add-surflet.scm")) 
                             "Make new calculation.") (br)
			     (a (@ (href  "/"))
				"Return to main menu."))))))
      "This string will never be evaluated.")
    
    ))
	 