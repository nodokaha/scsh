(define-structure surflet surflet-interface
  (open surflets
	surflets/my-input-fields
	scheme-with-scsh)
  (begin

    (define (make-nibble-input-fields)
      (let ((checkboxes (list (make-annotated-checkbox 8)
			      (make-annotated-checkbox 4)
			      (make-annotated-checkbox 2)
			      (make-annotated-checkbox 1))))
	(make-multi-input-field
	 #f "nibble-input"
	 (lambda (input-field bindings)
	   (let loop ((sum 0)
		      (checkboxes checkboxes))
	     (if (null? checkboxes)
		 sum
		 (loop (+ sum (or (input-field-value (car checkboxes) 
						     bindings)
				  0))
		       (cdr checkboxes)))))
	 '()
	 (lambda (ignore)
	   checkboxes))))

    (define nibble-input-field (make-nibble-input-fields))

    (define (main req)
      (let* ((req (send-html/suspend
		   (lambda (new-url)
		     `(html (title "Nibble Input Widget")
			    (body 
			     (h1 "Nibble Input Widget")
			     (p "Enter your nibble (msb left):")
			     (surflet-form ,new-url
					   ,nibble-input-field
					   ,(make-submit-button)))))))
	     (bindings (get-bindings req))
	     (number (input-field-value nibble-input-field bindings)))
	(send-html
	 `(html (title "Result")
		(body 
		 (h2 "Result")
		 (p "You've entered " ,number "."))))))
    ))
