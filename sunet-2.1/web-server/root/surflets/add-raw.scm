(define-structure surflet surflet-interface
  (open	surflet-requests		; SURFLET-REQUEST-url
	httpd-responses			; MAKE-RESPONSE
	parse-html-forms		; PARSE-HTML-FORM-QUERY
	url				; HTTP-url-SEARCH
	srfi-1				; FILTER
	surflet-handler/surflets	; SEND/SUSPEND, SEND/FINISH
	surflet-handler/primitives	; MAKE-SURFLET-RESPONSE
	scheme-with-scsh)
  (begin

    (define (extract-bindings key bindings)
      (map cdr
	   (filter (lambda (binding) 
		     (equal? (car binding) key))
		   bindings)))

    (define (extract-single-binding key bindings)
      (let ((bindings (extract-bindings key bindings)))
	(if (null? bindings)
	    (error "no binding")
	    (car bindings))))

    (define (make-get-number-page input-text title)
	(lambda (new-url)
	  (make-surflet-response
	   (status-code ok)
	   "text/html"
	   '()
	   (format #f "
<HTML>~a
<BODY>~a
<P>
  <FORM method=\"GET\" action=\"~a\">
    ~a
  <INPUT type=\"text\" name=\"number\">
  <INPUT type=\"submit\">
  </FORM>
</P>
<HR>
<P>
 <A href=\"/\">Return to main menu.</A><BR>
 <A href=\"add-raw.scm\">Start new calculation.</A>
</P>
</BODY>
</HTML>"
		   (if title 
		       (format #f "<TITLE>~a</TITLE>" title)
		       "")
		   (if title
		       (format #f "<H2>~a</H2>" title))
		   new-url
		   input-text
		   ))))

    (define (make-result-page new-url)
      (make-surflet-response
       (status-code ok)
       "text/html"
       '()
       (format #f "
<HTML>
<TITLE>Result</TITLE>
<BODY>
<H2>Result</H2>
<P>
  ~a
<P>
<HR>
<A href=\"add-raw.scm\">New calculation (new session)</A><BR>
<A href=\"javascript:history.go(-2)\">New calculation (same session)</A><BR>
<A href=~s>Close this session</A>
</BODY>
</HTML>"
	       (number->string (+ (get-number1) (get-number2)))
	       new-url)))
    
    (define (get-number input-text . maybe-title)
      (let* ((title (if (pair? maybe-title) (car maybe-title) #f))
	     (result (send/suspend (make-get-number-page input-text title)))
	     (bindings (parse-html-form-query
			(http-url-search (surflet-request-url result))))
	     (number (string->number 
		      (extract-single-binding "number" bindings))))
	(if number
	    number
	    (get-number input-text "Please enter a valid number"))))

    (define (get-number1)
      (get-number "First number:" "Calculation - Step one"))
    
    (define (get-number2)
      (get-number "Second number:" "Calculation - Step two"))
    
    (define (main req)
      (send/suspend make-result-page)
      ;; This finishes the session and does a redirect to the root
      ;; page.
      (send-error (status-code moved-temp) #f "/" "/"))

    ))
	 