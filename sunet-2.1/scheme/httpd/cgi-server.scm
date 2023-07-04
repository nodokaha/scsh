;;; Server support for NCSA's WWW Common Gateway Interface -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; See http://hoohoo.ncsa.uiuc.edu/cgi/interface.html for a sort of "spec".

;;; PROBLEMS: 
;;;   - The handlers could be made -- closed over their parameters
;;;     (e.g., root vars, etc.)

;;; This code provides a request handler for the HTTP server that implements 
;;; a CGI interface to external programs for doing HTTP transactions.

;;; About HTML forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This info is in fact independent of CGI, but important to know about, 
;;; as many CGI scripts are written for responding to forms-entry in
;;; HTML browsers.
;;;
;;; The form's field data are turned into a single string, of the form
;;;     name=val&name=val
;;; where the <name> and <val> parts are URI encoded to hide their
;;; &, =, and + chars, among other things. After URI encoding, the
;;; space chars are converted to + chars, just for fun. It is important
;;; to encode the spaces this way, because the perfectly general %xx escape
;;; mechanism might be insufficiently confusing. This variant encoding is
;;; called "form-url encoding."
;;;
;;; If the form's method is POST,
;;;     Browser sends the form's field data in the entity block, e.g.,
;;;     "button=on&ans=yes". The request's Content-type: is application/
;;; 	x-www-form-urlencoded, and the request's Content-length: is the
;;; 	number of bytes in the form data.
;;;
;;; If the form's method is GET,
;;;     Browser sends the form's field data in the URL's <search> part.
;;;     (So the server will pass to the CGI script as $QUERY_STRING,
;;;     and perhaps also on in argv[]).
;;;
;;; In either case, the data is "form-url encoded" (as described above).

;;; ISINDEX queries:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (Likewise for ISINDEX URL queries from browsers.)
;;; Browser url-form encodes the query (see above), which then becomes the
;;; ?<search> part of the URI. (Hence the CGI script will split the individual
;;; fields into argv[].)


;;; CGI interface:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - The URL's <search> part is assigned to env var $QUERY_STRING, undecoded.
;;; - If it contains no raw "=" chars, it is split at "+" chars. The
;;;   substrings are URI decoded, and become the elts of argv[].
;;; - The CGI script is run with stdin hooked up to the socket. If it's going
;;;   to read the entity, it should read $CONTENT_LENGTH bytes worth.
;;; - A bunch of env vars are set; see below.
;;; - If the script begins with "nph-" its output is the entire response.
;;;   Otherwise, it replies to the server, we peel off a little header
;;;   that is used to construct the real header for the response.
;;; See the "spec" for further details. (URL above).
;;;
;;; The "spec" also talks about PUT, but when I tried this on a dummy script,
;;; the NSCA httpd server generated buggy output. So I am only implementing
;;; the POST and GET ops; any other op generates a "405 Method not allowed"
;;; response.

;;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; path for scripts 
(define cgi-default-bin-path "/bin:/usr/bin:/usr/ucb:/usr/bsd:/usr/local/bin")

;;; The request handler for CGI scripts. (car path) is the script to run.
;;; cgi-bin-path is used, if PATH-variable isn't defined

(define (cgi-handler bin-dir . maybe-cgi-bin-path)
  (let-optionals
      maybe-cgi-bin-path
      ((cgi-bin-path cgi-default-bin-path))
    
    (let ((request-invariant-cgi-env    ; environment variables that never change
	   `(("PATH"              . ,cgi-bin-path)
	     ("SERVER_SOFTWARE"   . ,sunet-version-identifier)
	     ("SERVER_NAME"       . ,(host-info:name (host-info (system-name))))
	     ("GATEWAY_INTERFACE" . "CGI/1.1"))))
      (lambda (path req)
	(if (pair? path)	; Got to have at least one elt.
	    (compute-cgi path req bin-dir request-invariant-cgi-env)
	    (make-error-response (status-code bad-request) req "Empty CGI script"))))))

(define (compute-cgi path req bin-dir request-invariant-cgi-env)
  (let* ((prog (car path))

	 (filename (or (dotdot-check bin-dir (list prog))
		       (http-error (status-code bad-request) req 
				   "CGI scripts may not contain \"..\" elements.")))

	 (nph? (string-prefix? "nph-" prog))	; PROG starts with "nph-" ? 
					; why did we had (string-suffix? "-nph" prog) here?

	 (search (http-url-search (request-url req)))	; Compute the
	 (argv (if (and search (not (string-index search #\=)))	; argv list.
		   (split-and-decode-search-spec search)
		   '()))

	 (env (cgi-env req bin-dir (cdr path) request-invariant-cgi-env))

	 (doit (lambda ()
		 (dup->inport  (socket:inport (request-socket req)) 0)
		 (dup->outport (current-output-port) 1)
		 (dup 1 2)
		 (apply exec/env filename env argv))))

    (http-syslog (syslog-level debug) "[cgi-server] search: ~s, argv: ~s~%" search argv)
    (let ((request-method (request-method req)))
      (cond 
       ((or (string=? request-method "GET")
	    (string=? request-method "POST"))	; Could do others also.
	(case (file-not-executable? filename)
	  ((search-denied permission)
	   (make-error-response (status-code forbidden) req
				"Permission denied."))
	  ((no-directory nonexistent)
	   (make-error-response (status-code not-found) req
				"File or directory doesn't exist."))
	  (else
	   (if nph?
	       (cgi-make-nph-response (run/port* doit))
	       (cgi-make-response (run/port* doit) path req)))))
       
       (else 
	(make-error-response (status-code method-not-allowed) req request-method))))))


(define (split-and-decode-search-spec s)
  (let recur ((i 0))
    (cond
     ((string-index s #\+ i) => (lambda (j) (cons (unescape-uri s i j)
						  (recur (+ j 1)))))
     (else (list (unescape-uri s i (string-length s)))))))


;;; Compute the CGI scripts' process environment by adding the standard CGI
;;; environment var bindings to the current process env -- return result
;;; as an alist.
;;;
;;; You are also supposed to add the headers as env vars in a particular
;;; format, but are allowed to bag it if the environment var storage 
;;; requirements might overload the OS. I don't know what you can rely upon
;;; in Unix, so I am just bagging it, period.
;;;
;;; Suppose the URL is
;;;   //machine/cgi-bin/test-script/foo/bar?quux%20a+b=c
;;; then:
;;   PATH_INFO -- extra info after the script-name path prefix. "/foo/bar"
;;;   PATH_TRANSLATED -- non-virtual version of above.  "/u/Web/foo/bar/"
;;;   SCRIPT_NAME virtual path to script "/cgi-bin/test-script"
;;;   QUERY_STRING -- not decoded "quux%20a+b=c"
;;; The first three of these vars are *not* encoded, so information is lost
;;; if the URL's path elements contain encoded /'s (%2F). CGI loses.

(define (cgi-env req bin-dir path-suffix request-invariant-cgi-env)
  (let* ((sock (request-socket req))
	 (raddr (socket-remote-address sock))

	 (headers (request-headers req))

	 ;; Compute the $PATH_INFO and $PATH_TRANSLATED strings.
	 (path-info (uri-path->uri path-suffix)) ; No encode or .. check.
	 (path-translated (path-list->file-name path-info bin-dir))

	 ;; Compute the $SCRIPT_PATH string.
	 (url-path (http-url-path (request-url req)))
	 (script-path (take (- (length url-path) (length path-suffix))
			    url-path))
	 (script-name (uri-path->uri script-path)))

    (receive (rhost rport)
	     (socket-address->internet-address raddr)
      (receive (lhost lport)
               (socket-address->internet-address (socket-local-address sock))

	`(("SERVER_PROTOCOL" . ,(version->string (request-version req)))
	  ("SERVER_PORT"     . ,(number->string lport))
	  ("REQUEST_METHOD"  . ,(request-method req))

	  ("PATH_INFO"       . ,path-info)
	  ("PATH_TRANSLATED" . ,path-translated)
	  ("SCRIPT_NAME"     . ,script-name)

	  ("REMOTE_ADDR"  . ,(format-internet-host-address rhost))

	  ;; ("AUTH_TYPE" . xx)			; Random authentication
	  ;; ("REMOTE_USER" . xx)		; features I don't understand.
	  ;; ("REMOTE_IDENT" . xx)

	  ,@request-invariant-cgi-env	; Stuff that never changes (see cgi-handler).

	  ,@(cond ((http-url-search (request-url req)) =>
		   (lambda (srch) `(("QUERY_STRING" . ,srch))))
		  (else '()))

	  ,@(cond ((get-header headers 'content-type) =>
		   (lambda (ct) `(("CONTENT_TYPE" . ,ct))))
		  (else '()))

	  ,@(cond ((get-header headers 'content-length) =>
		   (lambda (cl)	; Skip initial whitespace (& other non-digits).
		     (let ((first-digit (string-index cl char-set:digit))
			   (cl-len (string-length cl)))
		       (if first-digit
			   `(("CONTENT_LENGTH" . ,(substring cl first-digit cl-len)))
			   (http-error (status-code bad-request) req 
				       "Illegal `Content-length:' header.")))))
		  
		  (else '()))
	  
	  . ,(env->alist))))))


(define (take n lis)
  (if (zero? n) '()
      (cons (car lis) (take (- n 1) (cdr lis)))))

(define (drop n lis)
  (if (zero? n) lis
      (drop (- n 1) (cdr lis))))


;;; Script's output for request REQ is available on SCRIPT-PORT.
;;; The script isn't an "nph-" script, so we read the response, and mutate
;;; it into a real HTTP response, which we then send back to the HTTP client.

(define (cgi-make-response script-port path req)
  (set-port-buffering script-port bufpol/block 4096)
  (let* ((headers (read-rfc822-headers script-port))
	 (ctype (get-header headers 'content-type))
	 (loc   (get-header headers 'location))
	 (stat (cond ((get-header headers 'status)
		      => (lambda (code.text)
			   (extract-status-code-and-text code.text
							 req)))
		     (else
		      (http-syslog (syslog-level notice) 
				   "CGI script didn't generate status header.")
		      (cons 200 "OK"))))
	 (extra-headers (delete-headers (delete-headers (delete-headers headers
									'content-type)
							'location)
					'status)))
    
    (http-syslog (syslog-level debug) "[cgi-server] headers: ~s~%" headers)
    (http-syslog (syslog-level debug) "[cgi-server] request-method=~a~%" 
		 (request-method req))

    (if loc
	(if (uri-has-protocol? (string-trim loc))
	    (make-error-response (status-code moved-perm) req
				      loc loc)
	    (make-redirect-response (string-trim loc)))
	;; Send the response header back to the client
	(make-response ;code message seconds mime extras body
	 (number->status-code (car stat))
	 (cdr stat)			; text
	 (time)
	 (or ctype "text/html")
	 extra-headers
	 (make-writer-body
	  (lambda (out options)
	    (copy-inport->outport script-port out 4096)
	    (close-input-port script-port)))))))

(define (delete-headers headers tag)
  (alist-delete tag headers))

(define (cgi-make-nph-response script-port)
  (make-nph-response
   (make-writer-body (lambda (out options)
		       (copy-inport->outport script-port out)))))

(define (uri-has-protocol? loc)
  (receive (proto path search frag)
      (parse-uri loc)
    (if proto #t #f)))

(define (extract-status-code-and-text status req)
  (with-fatal-error-handler*
   (lambda (c d)
     (http-error (status-code bad-gateway) req
		 "CGI script generated an invalid status header."
		 status c))
   (lambda ()
     (let ((status (string-trim status)))
       (cons (string->number (substring status 0 3)) ; number
	     (substring/shared status 4)))))) ; text
