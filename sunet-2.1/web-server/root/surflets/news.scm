(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets)
  (begin
    (define *news* '())
    
    (define (read-news news-file-name)
      (close-after (open-input-file news-file-name)
	(lambda (news-input)
	  (let loop ((next-line (read-line news-input))
		     (news '()))
	    (if (eof-object? next-line)
		news
		(loop (read-line news-input)
		      (cons next-line news)))))))
      
    (define (main req)
      (if (null? *news*) 
	  (set! *news* (read-news "news.txt")))
      (let loop ((news *news*))
	(if (null? news)
	    (show-final-page)
	    (begin
	      (show-news-page (car news))
	      (loop (cdr news))))))

    (define (show-final-page)
      (send-html/finish 
       `(html (body (p (h1 "THAT'S IT"))
		    (p ("That's it..."))
		    (hr)
		    (p (url "news.scm" "See news again.") (br)
		       (url "/" "Return to main menu."))))))

    (define (show-news-page news)
      (send-html/suspend 
       (lambda (next-url)
	 `(html (body (p (h1 ,news))
		      (a (@ href ,next-url) "read more...")
		      (hr)
		      (p (url "news.scm" "See news again from beginning.") (br)
			 (url "/" "Return to main menu.")))))))
    ))

