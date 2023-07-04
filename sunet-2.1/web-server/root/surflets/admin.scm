(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	surflet-handler/admin
	)
  (begin

    (define (main-page)
      `(html (title "SUrflet Administration")
	     (body (h1 "SUrflet Administration Menu")
		   (p "This SUrflet allows you to do some adminstration tasks.")
		   (p "Choose one of the following submenus:")
		   (p
		    (ul
		     (li (url "admin-handler.scm" "Set handler options..."))
		     (li (url "admin-surflets.scm" "SUrflets..."))
		     (li (url "admin-profiling.scm" "Profiling..."))))
		   (hr)
		   (p (url "/" "Return to main menu.")))))

    (define (main req)
      (send-html/finish (main-page)))

    ))