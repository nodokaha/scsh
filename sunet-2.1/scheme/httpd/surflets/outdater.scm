;;; Copyright 2002, 2003 Andreas Bernauer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outdater

(define-record-type outdater :outdater
  (real-make-outdater outdated?)
  outdater?
  (outdated? outdater-outdated? set-outdater-outdated?!))

(define (make-outdater)
  (real-make-outdater #f))

(define-syntax if-outdated
  (syntax-rules ()
    ((if-outdated outdater consequence alternative)
     (if (outdater-outdated? outdater)
	 consequence
	 (begin
	   (set-outdater-outdated?! outdater #t)
	   alternative)))))

(define (show-outdated url)
  (send-html 
   `(html (title "Outdated Data")
	  (body (h1 "Outdated Data")
		(p "The page or action you requested relies on outdated data.")
		,(if url
		     `(p "Try to " 
			 (url ,url "reload") 
			 " the page to get current data.")
		     '())))))
