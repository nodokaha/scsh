(define-structure surflet surflet-interface
  (open scheme-with-scsh
	handle-fatal-error
	let-opt
	surflets
	surflets/error
	surflet-handler/options
	)
  (begin

    (define (get-option-change update-text options)
      (send-html/suspend
       (lambda (new-url)
	 `(html 
	   (title "SUrflet Adminstration - Handler options")
	   (body 
	    (h1 "SUrflet Administration")
	    (h2 "Handler options")
	    ,(and (pair? update-text) update-text)
	    (p "These are the runtime configurable options of the handler:")
	    (surflet-form 
	     ,new-url
	     POST
	     (table
	      ,@(map (lambda (option)
		       (let ((text (car option))
			     (input-field (cadr option))
			     (submit-button (caddr option)))
			 `(tr (td ,text) 
			      (td ,input-field)
			      (td ,submit-button))))
		     options)))
	    (hr)
	    (p (url "admin.scm" "Return to adminstration menu.") (br)
	       (url "/" "Return to main menu."))))
	 )))
    
    (define submit-timeout (make-submit-button "Change"))
    (define submit-cache (make-submit-button "Change"))

    (define (handler-options req . maybe-update-text)
      (let* ((update-text `(font (@ (color "red"))
				 ,(:optional maybe-update-text "")))
	     (number-field 
	      (make-number-field (options-session-lifetime)))
	     (cache-checkbox (make-checkbox (options-cache-surflets?)))
	     (options `(("Current session lifetime: " ,number-field ,submit-timeout)
			("Cache SUrflets?" ,cache-checkbox ,submit-cache)))
	     (req (get-option-change update-text options))
	     (bindings (get-bindings req)))
	(cond 
	 ((returned-via? submit-timeout bindings)
	   (let ((result (input-field-value number-field bindings)))
	     (if result
		 (if (and (integer? result)
			  (> result 0))
		     (begin
		       (set-options-session-lifetime! result)
		       (handler-options req 
					(format #f "Session lifetime changed to ~a." 
						(options-session-lifetime))))
		     (error "not a positive integer"))
		 (handler-options req "Please enter a valid, positive integer number"))))
	 ((returned-via? submit-cache bindings)
	  (let ((cache-plugins? (input-field-value cache-checkbox bindings)))
	    (set-options-cache-surflets?! cache-plugins?)
	    (handler-options req 
			     (format #f "Caching turned ~s." 
				     (if cache-plugins? "on" "off")))))
	 (else
	  (error "unexpected return" bindings)))))

    (define (main req)
      (handler-options req))

    ))