(define-structure surflet surflet-interface
  (open surflets
	surflets/utilities		;form-query-list
	surflet-requests
	surflet-handler/primitives
	httpd-responses
	url
	scheme-with-scsh)
  (begin

    (define (get-number input-text . maybe-title)
      (let* ((title (if (pair? maybe-title) (car maybe-title) #f))
	     (result 
	      (send-html/suspend
	       (lambda (new-url)
		 `(html ,(if title
			     `(title ,title)  '())
			(body
			 ,(if title `(h2 ,title) '())
			 (p
			  (form (@ (method "get")
				   (action ,new-url))
				,input-text " "
				(input (@ (type "text")
					  (name "number"))
				       (input (@ (type "submit"))))))
			 (hr)
			 (p (url "/" "Return to main menu") (br)
			    (url "add-html.scm" "Start new calculation."))))))))
	(let* ((bindings (form-query-list
			  (http-url-search (surflet-request-url result))))
	       (number (string->number 
			(extract-single-binding "number" bindings))))
	  (if number
	      number
	      (get-number input-text "Please enter a valid number")))))

    (define (get-number1)
      (get-number "First number:" "Calculation - Step one"))

    (define (get-number2)
      (get-number "Second number:" "Calculation - Step two"))

    (define (main req)
      (let ((req 
	    (send-html/suspend
	     (lambda (new-url)
	       `(html (title "Result")
		      (body (h2 "Result")
			    (p ,(number->string (+ (get-number1) (get-number2))))
			    (hr)
			    (a (@ (href "add-html.scm")) "New calculation (new session)")(br)
			    (a (@ (href "javascript:history.go(-2)")) "New calculation (same session)")(br)
			    (a (@ (href ,new-url)) "Close this session")))))))
	;; How to clear session data and go to another HTML page:
	(send-error (status-code moved-temp) req 
		    "/" "/")
	))
;	))
    ))
	 