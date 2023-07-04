;;; http server in the Scheme Shell	-*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; Copyright (c) 1996-2002 by Mike Sperber.
;;; Copyright (c) 2000-2002 by Martin Gasbichler.
;;; Copyright (c) 2002 by Andreas Bernauer.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.


;;; This file implements the core of an HTTP server: code to establish
;;; net connections, read and parse requests, and handler errors. 
;;; It does not have the code to actually handle requests. That's up
;;; to other modules, and could vary from server to server. To build
;;; a complete server, you need to define request handlers (see below) --
;;; they determine how requests are to be handled.
;;;
;;; The RFC detailing the HTTP 1.0 protocol, RFC 1945, can be found at
;;; http://www.w3.org/Protocols/rfc1945/rfc1945


(define server/protocol "HTTP/1.0")

(define (httpd options)
  (let ((port (httpd-options-port options))
	(root-dir (httpd-options-root-directory options))
	(rate-limiter
	 (cond
	  ((httpd-options-simultaneous-requests options)
	   => make-rate-limiter)
	  (else #f))))
    (let-thread-fluid 
     logging 
     (make-logging)
     (lambda ()

       (init-http-log! options)
       (with-syslog-destination
	"httpd" #f #f #f
	(lambda ()
	  (with-cwd
	   root-dir
	   (bind-prepare-listen-accept-loop
	    protocol-family/internet
	    (lambda ()
	      (cond ((httpd-options-post-bind-thunk options)
		     => (lambda (thunk)
			  (thunk)))))
	    (lambda (sock addr)
	      (if rate-limiter
		  (begin
		    (rate-limit-block rate-limiter)
		    (rate-limit-open rate-limiter)))

	      (with-fatal-error-handler
	       (lambda (c decline)
		 (http-syslog (syslog-level notice) "error during connection negotiation~%")
		 (if rate-limiter
		     (rate-limit-close rate-limiter)))
	       (call-with-values
		(lambda ()
		  (socket-address->internet-address (socket-remote-address sock)))
		(lambda (host-address service-port)
		  (if (and rate-limiter (http-syslog?))
		      (http-syslog (syslog-level info) "<~a>~a: concurrent request #~a~%"
				   (pid)
				   (format-internet-host-address host-address)
				   (rate-limiter-current-requests rate-limiter)))

		  (set-port-buffering (socket:outport sock) bufpol/block 4096)
		  (fork-thread 
		   (lambda ()
                     ;; If there is buffering for the input, 
                     ;; CGI scripts don't get the full request
		     (set-port-buffering (socket:inport sock) bufpol/none)
		     (process-toplevel-request sock host-address options)
		     (if (http-syslog?)
			 (http-syslog (syslog-level debug) "<~a>~a [closing]~%"
				      (pid)
				      (format-internet-host-address host-address)))
		     (with-fatal-error-handler
		      (lambda (c decline)
			(if (http-syslog?)
			    (http-syslog (syslog-level notice) "<~a>~a [error closing (~a)]~%"
					 (pid)
					 (format-internet-host-address host-address)
					 c)))
		      (close-socket sock))
		     (if rate-limiter
			 (rate-limit-close rate-limiter))
		     (if (http-syslog?)
			 (http-syslog (syslog-level info) "<~a>~a [closed]~%"
				      (pid)
				      (format-internet-host-address host-address)))))))))
	    port))))))))


;;; Top-level http request processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read, parse, and handle a single http request. The only thing that makes
;;; this complicated is handling errors -- as a server, we can't just let the
;;; standard error handlers toss us into a breakpoint. We have to catch the
;;; error, send an error response back to the client if we can, and then keep
;;; on trucking. This means using the S48's condition system to catch and
;;; handle the various errors, which introduces a major point of R5RS
;;; incompatibiliy -- R5RS has no exception system. So if you were to port
;;; this code to some other Scheme, you'd really have to sit down and think
;;; about this issue for a minute.

(define (process-toplevel-request sock host-address options)
  ;; This top-level error-handler catches *all* uncaught errors and warnings.
  ;; If the error condition is a reportable HTTP error, we send a response back 
  ;; to the client. In any event, we abort the transaction, and return from
  ;; PROCESS-TOPLEVEL-REQUEST.
  ;;
  ;; We *oughta* map non-http-errors into replies anyway.
  (with-fatal-error-handler*
   (lambda (c decline)
     (http-syslog (syslog-level notice) "<~a>~a: error: ~s~%"
		  (pid)
		  (format-internet-host-address host-address)
		  c)
     (with-fatal-error-handler*
      (lambda (c decline)
	(http-syslog (syslog-level notice) "<~a>~a [error shutting down: ~s]~%"
		     (pid)
		     (format-internet-host-address host-address)
		     c))
      (lambda ()
	(shutdown-socket sock shutdown/sends+receives)
	(http-syslog (syslog-level info) "<~a>~a [shut down]~%"
		     (pid)
		     (format-internet-host-address host-address)))))
   (lambda ()
     (call-with-values
      (lambda ()
	(with-fatal-error-handler*
	 (lambda (c decline)
	   (http-syslog (syslog-level notice) "<~a>~a: error: ~s~%"
			(pid)
			(format-internet-host-address host-address)
			c)
	   (cond
	    ((http-error? c)
	     (apply (lambda (status-code req . args)
		      (values req
			      (apply make-error-response
				     status-code req
				     args)))
		    (condition-stuff c)))
	    ((fatal-syntax-error? c)
	     (values #f
		     (apply make-error-response (status-code bad-request)
			    #f		; No request yet.	
			    "Request parsing error -- report to client maintainer."
			    (condition-stuff c))))
	    ((not (and (exception? c)
		       (eq? (exception-reason c)
			    (enum exception os-error))))

	     ;; try to send bug report to client
	     (values #f
		     (apply make-error-response (status-code internal-error) 
			    #f		; don't know
			    "Internal error occured while processing request"
			    c)))
	    (else
	     (decline))))
	 (lambda ()
	   (let ((initial-req (parse-http-request sock options)))
	     (let redirect-loop ((req initial-req))
	       (let response-loop ((response ((httpd-options-request-handler options)
					      (http-url-path (request-url req))
					      req)))
		 (cond
		  ((input-response? response)
		   (response-loop
		    ((input-response-body-maker response)
		     (socket:inport sock))))
		  ((nph-response? response)
		   (values req response))
		  ((eq? (response-code response) (status-code redirect))
		   (redirect-loop (redirect-request req response sock options)))
		  (else
		   (values req response)))))))))
      (lambda (req response)
	
	(send-http-response req response 
			    (socket:inport sock)
			    (socket:outport sock)
			    options)
	)))))

(define (redirect-request req response socket options)
  (let* ((new-location-uri (redirect-body-location (response-body response)))
	 (url (with-fatal-error-handler*
	       (lambda (c decline)
		 (if (fatal-syntax-error? c)
		     (http-error (status-code internal-error) req
				 (format #f "Bad redirection out from CGI program: ~%~a"
					 (cdr c)))
		     (decline c)))
	       (lambda ()
		 ;; (future) NOTE: With this, a redirection may change the
		 ;; protocol in use (currently, the server only supports one of
		 ;; it). This might be inapplicable.
		 (parse-http-servers-url-fragment new-location-uri socket options)))))
    
    (make-request "GET"
		  new-location-uri
		  url
		  (request-version req)	; did not change
		  '()			; no rfc822 headers
		  (request-socket req))))

;;;; HTTP request parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This code provides procedures to read requests from an input
;;;; port.

;;; Read and parse an http request from INPORT.
;;; 
;;; Note: this parser parses the URI into an http URL record. If the URI
;;; isn't an http URL, the parser fails. This may not be right. There's
;;; nothing in the http protocol to prevent you from passing a non-http
;;; URI -- what this would mean, however, is not clear. Like so much of
;;; the Web, the protocols are redundant, underconstrained, and ill-specified.

(define (parse-http-request sock options)
  (let ((line (read-crlf-line (socket:inport sock))))
    ;; Blat out some logging info.
    (if (http-syslog?)
	(call-with-values
	 (lambda ()
	   (socket-address->internet-address (socket-remote-address sock)))
	 (lambda (host-address service-port)
	   (http-syslog (syslog-level info) "<~a>~a: ~a~%"
		     (pid)
		     (format-internet-host-address host-address)
		     line))))
    
    (if (eof-object? line)
	(fatal-syntax-error "EOF while parsing request.")
	
	(let* ((elts (string->words line))	; Split at white-space.
	       (version (case (length elts)
			  ((2) '(0 . 9))
			  ((3) (parse-http-version (caddr elts)))
			  (else (fatal-syntax-error "Bad Request Line."))))
	       (meth (car elts))
	       (uri-string (cadr elts))
	       (url (parse-http-servers-url-fragment uri-string sock options))
	       (headers (if (equal? version '(0 . 9))
			    '()
			    (read-rfc822-headers (socket:inport sock)))))
	  (make-request meth uri-string url version headers sock)))))

;;; Parse the URL, but if it begins without the "http://host:port"
;;; prefix, interpolate one from SOCKET. It would be sleazier but
;;; faster if we just computed the default host and port at
;;; server-startup time, instead of on every request.
;;; REDIRECT-REQUEST relys on that nothing is read out from SOCKET.

(define (parse-http-servers-url-fragment uri-string socket options)
  (receive (scheme path search frag-id) (parse-uri uri-string)
    (if frag-id				; Can't have a #frag part.
	(fatal-syntax-error "HTTP URL contains illegal #<fragment> suffix."
			    uri-string)

	(if scheme
	    (if (string-ci=? scheme "http") ; Better be an http url.
		(parse-http-url path search #f)
		(fatal-syntax-error "Non-HTTP URL" uri-string))

	    ;; Interpolate the server struct from our net connection.
	    (if (and (pair? path) (string=? (car path) ""))
		(let* ((addr (socket-local-address socket))
		       (local-name (or (httpd-options-fqdn options)
				       (socket-address->fqdn addr)))
		       (portnum (or (httpd-options-reported-port options)
				    (my-reported-port addr))))
		  (make-http-url (make-server #f #f
						local-name
						(number->string portnum))
				 (map unescape-uri (cdr path)) ; Skip initial /.
				 search
				 #f))

		(fatal-syntax-error "Path fragment must begin with slash"
				    uri-string))))))


(define parse-http-version
  (let ((re (make-regexp "^HTTP/([0-9]+)\\.([0-9]+)$"))
	(lose (lambda (s) (fatal-syntax-error "Bad HTTP version" s))))
    (lambda (vstring)
      (let ((m (regexp-exec re vstring)))
	(if m
	    (cons (or (string->number (match:substring m 1) 10) (lose vstring))
		  (or (string->number (match:substring m 2) 10) (lose vstring)))
	    (lose vstring))))))
				      

;;; Split string into a list of whitespace-separated strings.
;;; This could have been trivially defined in scsh as (field-splitter " \t\n")
;;; but I hand-coded it because it's short, and I didn't want invoke the
;;; regexp machinery for something so simple.

(define non-whitespace (char-set-complement char-set:whitespace))

(define (string->words s)
  (let recur ((start 0))
    (cond ((string-index s non-whitespace start) =>
        (lambda (start)
	  (cond ((string-index s char-set:whitespace start) => 
	      (lambda (end)
		(cons (substring s start end)
		      (recur end))))
	     (else (list (substring s start (string-length s)))))))
       (else '()))))

(define (send-http-headers response port)
  (display server/protocol port)
  (write-char #\space port)
  (display (status-code-number (response-code response)) port)
  (write-char #\space port)
  (display (or (response-message response)
	       (status-code-message (response-code response)))
	   port)
  (write-crlf port)

  (send-http-header-fields
   (list (cons 'server (string-append "Scheme Untergrund " sunet-version-identifier))
	 (cons 'content-type (response-mime response))
	 (cons 'date (rfc822-time->string (response-seconds response))))
   port)
  (send-http-header-fields (response-extras response) port)

  (write-crlf port))


(define (send-http-response request response input-port output-port options)
  (cond
   ;;if request-record could not be built (i.e. either
   ;;fatal-syntax-error was called because of an erroneous request
   ;;line, or an server-internal error (not an os-error) occurred)
   ;;and therefore HTTP-version of request is not known, answer
   ;;with HTTP/1.0
   ((not request)
    (send-http-headers response output-port)
    (display-http-body (response-body response) input-port output-port options))
    ;;no CLF-logging)
   ((nph-response? response)
    (display-http-body (nph-response-body response) input-port output-port options)
    (http-log request (status-code ok))); guess the status code
   (else 
    (if (not (v0.9-request? request))
	(send-http-headers response output-port))
    (if (not (string=? (request-method request) "HEAD"))
	(display-http-body (response-body response) input-port output-port options))
    (http-log request (response-code response)))))

(define (send-http-header-fields headers port)
  (for-each (lambda (pair)
	      (display (car pair) port)
	      (write-char #\: port)
	      (display (cdr pair) port)
	      (write-crlf port))
	    headers))

(define (my-reported-port addr)
  (receive (ip-addr portnum) (socket-address->internet-address addr)
    portnum))

