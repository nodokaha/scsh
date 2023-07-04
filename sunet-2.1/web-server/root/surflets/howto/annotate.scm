(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    (define (main req)
      (let* ((select-input-field 
	      (make-select
	       (map make-annotated-select-option
		    '("Icecream" "Chocolate" "Candy")
		    '(1.5 2.0 0.5))))
	      (req (send-html/suspend
		    (lambda (k-url)
		      `(html 
			(head (title "Sweet Store"))
			(body
			 (h1 "Your choice")
			 (surflet-form 
			  ,k-url
			  (p "Select the sweet you want:"
			     ,select-input-field)
			  ,(make-submit-button)))))))
	      (bindings (get-bindings req))
	      (cost (input-field-value select-input-field bindings)))
	(send-html/finish
	 `(html (head (title "Receipt"))
		(body
		 (h2 "Your receipt:")
		 (p "This costs you $" ,cost "."))))))
))
