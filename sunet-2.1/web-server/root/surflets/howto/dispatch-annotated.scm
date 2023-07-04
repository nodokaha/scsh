(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (let* ((language (make-annotated-address))
	     (req (send-html/suspend
		   (lambda (k-url)
		     `(html 
		       (head (title "Multi-lingual"))
		       (body 
			(h2 "Select your language:")
			(ul
			 (li (url ,(language k-url "Hello, how are you?") 
				  "English")
			 (li (url ,(language k-url "Hallo, wie geht es Ihnen?")
				  "Deutsch")))))))))
	     (bindings (get-bindings req)))
	(case-returned-via bindings
	  ((language) => result-page))))

    (define (result-page text)
      (send-html/finish
       `(html 
	 (head (title "Greeting"))
	 (body
	  (h2 ,text)))))
			
    ))
			
