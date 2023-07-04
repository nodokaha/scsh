(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (send-html/finish
       '(html (body (h1 "Hello, world!")))))
    ))
			
