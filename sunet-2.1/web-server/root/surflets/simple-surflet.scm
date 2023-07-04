(define-structure surflet surflet-interface
  (open scheme-with-scsh
	simple-surflet-api)
  (begin
    
    (define (main req)
      (let* ((answers 
	      (form-query
	       (list 
		(list 'name "Full Name")
		(list 'pwd (make-password "Password"))
		(list 're-pwd (make-password "Retype password"))
		(list 'yob (make-number "Year of birth"))
		(list 'mail? (make-boolean "Subscribe to mailing list"))
		(list 'payment (make-yes-no "Pay per" "bill" "card"))
		(list 'date-of-bill 
		      (make-radio "Pay at" 
				  (list "first" "middle" "end of month."))))))
	     )
	(if (string=? (extract/single 'pwd answers)
		      (extract/single 're-pwd answers))
	    (begin
	      (inform (format #f "Hi ~a, you're password is ~s, you were born in ~a, you ~a to the mailing list and you pay per ~a at ~a of month. Click continue to perform recording."
			      (extract/single 'name answers)
			      (extract/single 'pwd answers)
			      (extract/single 'yob answers)
			      (if (extract/single 'mail? answers)
				  "have subscribed"
				  "did not subscribe")
			      (extract/single 'payment answers)
			      (car ((infix-splitter) 
				    (extract/single 'date-of-bill answers)))))
	      (final-page "Data recorded."))
	    (begin
	      (inform (format #f "Hi ~a, you've misspelled your password. Go back and retype it." 
			      (extract/single 'name answers)))
	      (final-page "Your registration has been canceled.")))))

))
	