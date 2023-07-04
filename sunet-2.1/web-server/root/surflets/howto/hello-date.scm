(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (send-html/finish
       `(html (body (h1 "Hello, world!")
		    (p "The current date and time is "
		       ,(format-date "~H:~M:~S ~p ~m/~d/~Y" (date)))))))
    ))
			
