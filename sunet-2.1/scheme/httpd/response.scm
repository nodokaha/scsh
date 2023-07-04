;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; Copyright (c) 2002 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define-record-type http-response :http-response
  (make-response code message seconds mime extras body)
  response?
  (code response-code) ;;HTTP status code
  (message response-message);;reason phrase: textual description of
			    ;;status-code, or #f (-> server sends
			    ;;default reason phrase)
  (seconds response-seconds);;time the content was created
  (mime response-mime);;string indicating the MIME type of the response
  (extras response-extras);;assoc list with extra headers to be
			  ;;added to the response; its elements are
			  ;;pairs, each of which consists of a symbol
			  ;;representing the field name and a string
			  ;;representing the field value.
  (body response-body));; message-body

;; This is mainly for nph-... CGI scripts.
;; This means that the body will output the entire MIME message, not
;; just the part after the headers.

(define-record-type http-nph-response :http-nph-response
  (make-nph-response body)
  nph-response?
  (body nph-response-body))

(define-record-type http-input-response :http-input-response
  (make-input-response body-maker)
  input-response?
  (body-maker input-response-body-maker))

(define-record-type http-writer-body :http-writer-body
  (make-writer-body proc)
  writer-body?
  (proc writer-body-proc))

(define-record-type http-reader-writer-body :http-reader-writer-body
  (make-reader-writer-body proc)
  reader-writer-body?
  (proc reader-writer-body-proc))

(define-record-type http-redirect-body :http-redirect-body
  (make-redirect-body location)
  redirect-body?
  (location redirect-body-location))

(define (display-http-body body iport oport options)
  (cond
   ((writer-body? body)
    ((writer-body-proc body) oport options))
   ((reader-writer-body? body)
    ((reader-writer-body-proc body) iport oport options))))

(define-finite-type status-code :http-status-code
  (number message)
  status-code?
  status-codes
  status-code-name
  status-code-index
  (number status-code-number)
  (message status-code-message)
  (
   (ok			200 "OK")
   (created		201 "Created")
   (accepted		202 "Accepted")
   (prov-info		203 "Provisional Information")
   (no-content		204 "No Content")

   (mult-choice		300 "Multiple Choices")
   (moved-perm		301 "Moved Permanently")
   (moved-temp		302 "Moved Temporarily")
   (method		303 "Method (obsolete)")
   (not-mod		304 "Not Modified")

   (bad-request		400 "Bad Request")
   (unauthorized	401 "Unauthorized")
   (payment-req		402 "Payment Required")
   (forbidden		403 "Forbidden")
   (not-found		404 "Not Found")
   (method-not-allowed	405 "Method Not Allowed")
   (none-acceptable	406 "None Acceptable")
   (proxy-auth-required	407 "Proxy Authentication Required")
   (timeout		408 "Request Timeout")
   (conflict		409 "Conflict")
   (gone			410 "Gone")

   (internal-error	500 "Internal Server Error")
   (not-implemented	501 "Not Implemented")
   (bad-gateway		502 "Bad Gateway")
   (service-unavailable	503 "Service Unavailable")
   (gateway-timeout	504 "Gateway Timeout")
  
   (redirect             -301 "Internal redirect")))

(define (name->status-code name)
  (if (not (symbol? name))
      (call-error name->status-code (list name))
      (let loop ((i 0))
	(cond ((= i (vector-length status-codes))
	       #f)
	      ((eq? name
		    (status-code-name (vector-ref status-codes i)))
	       (vector-ref status-codes i))
	      (else
	       (loop (+ i 1)))))))

(define (number->status-code number)
  (if (not (number? number))
      (call-error number->status-code (list number))
      (let loop ((i 0))
	(cond ((= i (vector-length status-codes))
	       #f)
	      ((= number
		  (status-code-number (vector-ref status-codes i)))
	       (vector-ref status-codes i))
	      (else
	       (loop (+ i 1)))))))
	
;;; (make-error-response status-code req [message . extras])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; As a special case, request REQ is allowed to be #f, meaning we haven't
;;; even had a chance to parse and construct the request. This is only used
;;; for 400 BAD-REQUEST error report.

(define (make-error-response code req . args)
  (let* ((message (and (pair? args) (car args)))
	 (extras  (if (pair? args) (cdr args) '()))

	 (generic-title (lambda (port)
			  (title-html port
				      (status-code-message code))))
	 (send-message (lambda (port)
			 (if message 
			     (format port "<BR>~%Further Information: ~A<BR>~%" message))))
	 (close-html (lambda (port)
		       (for-each (lambda (x) (format port "<BR>~s~%" x)) extras)
		       (write-string "</BODY>\n" port)))
			
	 (create-response
	  (lambda (headers writer-proc)
	    (make-response code
			   #f
			   (time)
			   "text/html"
			   headers
			   (make-writer-body writer-proc)))))

    (cond
     ;; This error response requires two args: message is the new URI: field,
     ;; and the first EXTRA is the older Location: field.
     ((or (eq? code (status-code moved-temp))
	  (eq? code (status-code moved-perm)))
      (create-response
       (list (cons 'uri message)
	     (cons 'location (car extras)))
       (lambda (port options)
	 (title-html port "Document moved")
	 (format port
		 "This document has ~A moved to a <A HREF=\"~A\">new location</A>.~%"
		 (if (eq? code (status-code moved-temp))
		     "temporarily"
		     "permanently")
		 message)
	 (close-html port))))

     ((eq? code (status-code bad-request))
      (create-response
       '()
       (lambda (port options)
	 (generic-title port)
	 (write-string "<P>Client sent a query that this server could not understand.\n"
		       port)
	 (send-message port)
	 (close-html port))))

     ((eq? code (status-code method-not-allowed))
      (create-response
       '()
       (lambda (port options)
	 (generic-title port)
	 (write-string "<P>Method not allowed.\n" port)
	 (send-message port)
	 (close-html port))))

     ((eq? code (status-code unauthorized))
      (create-response
       (list (cons 'WWW-Authenticate message)) ; Vas is das? 
       ;; Vas das is? See: http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.47
       ;; message should be a challenge(?)
       (lambda (port options)
	 (title-html port "Authorization Required")
	 (write-string "<P>Browser not authentication-capable or\n" port)
	 (write-string "authentication failed.\n" port)
	 (send-message port)
	 (close-html port))))

     ((eq? code (status-code forbidden))
      (create-response
       '()
       (lambda (port options)
	 (title-html port "Request not allowed.")
	 (format port 
		 "Your client does not have permission to perform a ~A~%"
		 (request-method req))
	 (format port "operation on url ~a.~%" (request-uri req))
	 (send-message port)
	 (close-html port))))
       
     ((eq? code (status-code not-found))
      (create-response
       '()
       (lambda (port options)
	 (title-html port "URL not found")
	 (write-string 
	  "<P>The requested URL was not found on this server.\n"
	  port)
	 (send-message port)
	 (close-html port))))

     ((eq? code (status-code internal-error))
      (create-response
       '()
       (lambda (port options)
	 (generic-title port)
	 (format port "The server encountered an internal error or
misconfiguration and was unable to complete your request.
<P>
Please inform the server administrator, ~A, of the circumstances leading to
the error, and time it occured.~%"
		 (or (httpd-options-server-admin options)
		     "[no mail address available]"))
	 (send-message port)
	 (close-html port))))
      
     ((eq? code (status-code not-implemented))
      (create-response
       '()
       (lambda (port options)
	 (generic-title port)
	 (format port "This server does not currently implement 
the requested method (~A).~%"
		 (request-method req))
	 (send-message port)
	 (close-html port))))

     ((eq? code (status-code bad-gateway))
      (create-response
       '()
       (lambda (port options)
	 (generic-title port)
	 (format port "An error occured while waiting for the 
response of a gateway.~%")
	 (send-message port)
	 (close-html port)))))))

(define (title-html out message)
  (format out "<HEAD>~%<TITLE>~%~A~%</TITLE>~%</HEAD>~%~%" message)
  (format out "<BODY>~%<H1>~A</H1>~%" message))

;; Creates a redirect response. The server will serve the new file
;; indicated by NEW-LOCATION. NEW-LOCATION must be uri-encoded and
;; begin with a slash.  This is intended for CGI scripts. Note that
;; the browser won't notice the redirect. Thus, it will keep the
;; original URL. For "real" redirections, use 
;; (make-error-response (status-code moved-perm) req 
;;                      "new-location" "new-location").
(define (make-redirect-response new-location)
  (make-response
   (status-code redirect)
   #f
   (time)
   ""
   '()
   (make-redirect-body new-location)))
