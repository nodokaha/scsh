(define-structure surflet surflet-interface
  (open surflets
	surflets/callbacks
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (let ((language (make-annotated-callback result-page)))
	(send-html
	 `(html 
	   (head (title "Multi-lingual"))
	   (body 
	    (h2 "Select your language:")
	    (ul
	     (li (url ,(language "Hello, how are you?") 
		      "English")
		 (li (url ,(language "Hallo, wie geht es Ihnen?")
			  "Deutsch")))))))))

    (define (result-page req text)
      (send-html/finish
       `(html 
	 (head (title "Greeting"))
	 (body
	  (h2 ,text)))))
			
    ))
			
