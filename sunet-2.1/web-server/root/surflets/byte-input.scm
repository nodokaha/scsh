(define-structure surflet surflet-interface
  (open surflets
	surflet-requests
	surflets/my-input-fields
	handle-fatal-error
	url
	scheme-with-scsh)
  (begin

    (define (make-byte-input-fields bits)
      (let ((checkboxes 
	     (reverse
	      (let loop ((count 0)
			 (order 1))
		(if (= count bits)
		    '()
		    (cons 
		     (make-annotated-checkbox order)
		     (loop (+ 1 count)
			   (* 2 order))))))))

	(make-multi-input-field
	 #f "byte-input"
	 (lambda (input-field bindings)
	   (let loop ((sum 0)
		      (checkboxes checkboxes))
	     (if (null? checkboxes)
		 sum
		 (loop (+ sum (or (input-field-value (car checkboxes) bindings)
				  0))
		       (cdr checkboxes)))))
	 '()
	 (lambda (ignore)
	   checkboxes))))

    (define byte-input-fields (make-byte-input-fields 8))

    (define (show-result result)
      (send-html
       `(html (title "Result")
	      (body 
	       (h2 "Result")
	       (p "You've entered " ,result ".")
	       (hr)
	       (p (url "byte-input.scm" "Make new byte input.") (br)
		  (url "/" "Return to main menu."))))))

    (define (get-byte-input)
      (let* ((req (send-html/suspend
		   (lambda (new-url)
		     `(html (title "Byte Input Widget")
			    (body 
			     (h1 "Byte Input Widget")
			     (p "Enter your byte (msb left):")
			     (surflet-form ,new-url
					   ,byte-input-fields
					   ,(make-submit-button))
			     (hr)
			     (p (url "/" "Return to main menu.")))))))
	     (bindings (get-bindings req)))
	(input-field-value byte-input-fields bindings)))

    (define (main req)
      (show-result (get-byte-input)))
      

    ))