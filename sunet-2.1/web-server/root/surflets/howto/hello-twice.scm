(define-structure surflet surflet-interface
  (open surflets
	scheme-with-scsh)
  (begin
    
    (define (main req)
      (send-html/suspend
       (lambda (k-url)
	 `(html (body (h1 "Hello, world!")
		      (p (a (@ (href ,k-url)) "Next page -->"))))))

      (send-html/finish
       '(html (body (h1 "Hello, again!")))))
    ))
			
