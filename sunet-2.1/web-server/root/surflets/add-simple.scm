(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	simple-surflet-api
	)
  (begin

    (define (main req)
      (let* ((number-1 (single-query (make-number "First number:")))
	     (number-2 (single-query (make-number "Second number:"))))
	(inform (format #f "~a + ~a = ~a"
			number-1
			number-2
			(+ number-1 number-2))))
      (final-page "Session finished."))

))