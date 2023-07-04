(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	receiving
	srfi-1
	srfi-13
	srfi-14
	surflets/utilities
	surflets/callbacks
	httpd-responses)
  (begin
    (define global 0)

    (define selections (cons  '("a" "b" "c")
			      '("Andreas" "Bernd" "Clara")))
    (define radio-elements '(1 2 3 "a" *))
    (define select (make-annotated-select 
		    (map make-annotated-select-option
			 (car selections)
			 (cdr selections))
		    #t '(@ (size 2))))
    (define select2 (make-select (car selections)))
    (define text (make-text-field "yoho"))
    (define number (make-number-field 23))
    (define hidden (make-hidden-field "value"))
    (define password (make-password-field "asdf"))
    (define textarea (make-textarea "This
is 
a
test"))
    (define radio (make-annotated-radio-group))
    (define radios (map radio radio-elements))
    (define checkbox (make-annotated-checkbox "hooray!"))

    (define submit (make-submit-button))
    (define image  (make-image-button "/img/221.gif"))
    (define reset  (make-reset-button))

    (define (translate-line-breaks text)
      (let lp ((result '())
	       (text text))
	(let ((index (string-index text char-set:iso-control)))
	  (if index
	      (lp (cons '(br)
			(cons (substring/shared text 0 index) result))
		  ;; +2, as we probably have cr+lf
		  (substring/shared text (+ index 2)))
	      (reverse (cons text result))))))

    (define (cb-result req arg)
	(send-html
	 `(html (title "Result")
		(body (h2 "Result")
		      (p "Returned via callback with arg" (br)
			 ,(format #f "~s" arg))
		      (hr)
		      (p (url "test.scm" "Test again.") (br)
			 (url "/" "Return to main menu."))))))

    (define an-cb (make-annotated-callback cb-result))
    (define addr (make-annotated-address))
    (define (main req)
      (set! global (+ 1 global))
      (let* ((req (send-html/suspend
		   (lambda (new-url)
		     `((plain-html "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">")
		       (html 
			(head
			 (meta (@ (http-equiv "Content-Type")
				  (content "text/html; charset=ISO-8859-15")))
			(title "Test"))
		       (body 
			(h1 "This is from SUrflet")
			(p "called " ,global " times")
			(p "Choose an annotated address:")
			(ul
			 (li (url ,(addr new-url 13) "ab=ba"))
			 (li (url ,(addr new-url "be<ta") "be<ta"))
			 (li  (url ,(addr new-url) "<nothing>")))
			(p "Or choose an annotated callback")
			(ul
			 (li (url ,(an-cb 13) "13"))
			 (li (url ,(an-cb '(1 2 3)) "'(1 2 3)"))
			 (li (url ,(an-cb "hello") "hello"))
			 (li (url ,(an-cb #f) "#f")))
			(p "Or choose an input field.")
			(surflet-form
			 ,new-url
			 POST
			 (table
			  (tr (td "Selection:") (td ,select))
			  (tr (td "Selection2:") (td ,select2))
			  (tr (td "Simple text: ") (td ,text))
			  (tr (td "Number: " ) (td ,number))
			  (tr (td "Hidden: " ) (td ,hidden))
			  (tr (td "Password: " ) (td ,password))
			  (tr (td "Textarea: " ) (td ,textarea))
			  (tr (td "Radio:")
			      (td ,(zip radios 
					(map (lambda (elem)
					       (list (format #f "~%~s" elem)
						     '(nbsp) '(nbsp)))
					     radio-elements))))
			  (tr (td "Checkbox:") (td ,checkbox)))
			 ,submit ,reset (br) ,image)
			(hr)
			(p (url "/" "Return to main menu."))))))))
	     (bindings (get-bindings req))
	     (selected (input-field-value select bindings))
	     (selected2 (input-field-value select2 bindings))
	     (text-entered (input-field-value text bindings))
	     (number-entered (input-field-value number bindings))
	     (hidden-value (input-field-value hidden bindings))
	     (password-text (input-field-value password bindings))
	     (textarea-text (input-field-value textarea bindings))
	     (radio-result (input-field-value (radio #f) bindings))
	     (checkbox-result (input-field-value checkbox bindings))
	     (submit-result (returned-via? submit bindings))
	     (reset-result (returned-via? reset bindings))
	     (image-result (returned-via image bindings))
	     (result
	      (cond
	       ((returned-via? addr bindings) =>
		(lambda (string)
		  (format #f "returned via annotated string ~s" string)))
	       (else
		(set-text-field-value! text text-entered)
		(only-select-selected! select selected (cdr selections))
		(only-select-selected! select2 (list selected2) (car selections))
		(if number-entered
		    (set-number-field-value! number number-entered))
		(set-hidden-field-value! 
		 hidden 
		 (string-append "value" (number->string global)))
		(set-password-field-value! password password-text)
		(set-textarea-value! textarea textarea-text)
		(if radio-result
		    (begin
		      (map uncheck-radio! radios)
		      (check-radio! 
		       (list-ref radios
				 (list-index (lambda (a) (equal? a radio-result))
					     radio-elements)))))
		(if checkbox-result
		    (check-checkbox! checkbox)
		    (uncheck-checkbox! checkbox))
		`(p ,(cond
		      (image-result (format #f "Returned via image ~s" image-result))
		      (submit-result "Returned via submit")
		      (else "Don't know how you did return.")) (br)
		    "Bindings were: " ,(format #f "~s" bindings) (br)
		    (table 
		     (@ (valign "top"))
		     (tr (td "Selected: ") (td ,(format #f "~s" selected)))
		     (tr (td "Selected2:") (td ,(format #f "~s" selected2)))
		     (tr (td "Text entered:") (td ,(format #f "~s" text-entered)))
		     (tr (td "Number entered:") 
			 (td ,(if number-entered
				  number-entered
				  "no valid number")))
		     (tr (td "Hidden:") (td ,hidden-value))
		     (tr (td "Plain password:") (td ,password-text ))
		     (tr (td "Textarea:") 
			 (td #\" ,@(translate-line-breaks textarea-text) #\"))
		     (tr (td "Radio:") (td ,(format #f "~s" radio-result)))
		     (tr (td "Checkbox:") (td ,(format #f "~s" checkbox-result)))))
		    ))))
	(set! global (+ 1 global))
	(send-html/suspend
	 (lambda (continue)
	   `(html (body (h1 "Result")
			(p "called " ,global " times")
			,result (br)
			(url ,continue "show results again") (br)
			(url ,(make-callback main) "continue testing")
			(font (@ (size "small")) 
			      "(Note: This is not a browser history link)")
			(hr)
			(p (url "test.scm" "Test again.") (br)
			   (url "/" "Return to main menu."))))))
	
	(send-html/finish
	 `(html (body (h1 "Result 2")
		      (p "called " ,global " times")
		      ,(format #f "~s" (get-bindings req))
		      (hr)
		      (p (url "test.scm" "Test again.") (br)
			 (url "/" "Return to main menu.")))))))

    (define (only-select-selected! sel-if selected indices)
      (for-each (lambda (index)
		  (unselect-select-option! index sel-if))
		(iota (length (cdr selections))))
      (for-each (lambda (selected)
		  (select-select-option! 
		   (list-index (lambda (s) (string=? s selected))
			       indices)
		   sel-if))
		selected))

    ))
