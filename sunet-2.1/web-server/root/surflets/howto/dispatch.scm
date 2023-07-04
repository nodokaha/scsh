(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (let* ((english (make-address))
	     (german (make-address))
	     (req (send-html/suspend
		   (lambda (k-url)
		     `(html 
		       (head (title "Multi-lingual"))
		       (body 
			(h2 "Select your language:")
			(ul
			 (li (url ,(english k-url) "English")
			 (li (url ,(german k-url) "Deutsch")))))))))
	     (bindings (get-bindings req)))
	(case-returned-via bindings
	  ((english) (result-page "Hello, how are you?"))
	  ((german) (result-page "Hallo, wie geht es Ihnen?")))))

    (define (result-page text)
      (send-html/finish
       `(html 
	 (head (title "Greeting"))
	 (body
	  (h2 ,text)))))
			
    ))
			
