(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    (define (main req)
      (let* ((text-input (make-text-field))
	     (submit-button (make-submit-button))
	     (req (send-html/suspend
		   (lambda (k-url)
		     `(html 
		       (body 
			(h1 "Echo")
			(surflet-form ,k-url
				      (p "Please enter something:"
					 ,text-input
					 ,submit-button)))))))
	     (bindings (get-bindings req))
	     (user-input (input-field-value text-input bindings)))
	(send-html/finish
	 `(html (body
		 (h1 "Echo result")
		 (p "You've entered: '" ,user-input "'."))))))
))
