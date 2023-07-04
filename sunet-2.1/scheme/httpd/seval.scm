;;; Path handler for uploading Scheme code to the SU web server -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; This is really just an handler example demonstrating how to upload code 
;;; into the server.

;;; (do/timeout secs thunk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run THUNK, and gun it down if it hasn't finished in SECS seconds.
;;; Returns nothing useful, and THUNK gets executed in a subprocess,
;;; so its side-effects are invisible, as well. This is a clever kludge --
;;; it uses three subprocesses -- but I don't have interrupts, so I'm hosed.

(define (do/timeout* secs thunk)
  (run (begin (let ((timer  (fork (lambda () (sleep secs))))
		    (worker (fork thunk)))
		(receive (process status) (wait-any)
		  (ignore-errors
		   (lambda ()
		     (signal-process (proc:pid (if (eq? worker process)
						   timer
						   worker))
				     signal/kill))))))))
(define-syntax do/timeout
  (syntax-rules ()
    ((do/timeout secs body ...) (do/timeout* secs (lambda () body ...)))))

;;; The request handler for seval ops.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (seval-handler path req)
  (let ((request-method (request-method req)))
    (cond 
     ((string=? request-method "POST")	; Could do others also.
      (seval path req))
     (else
      (make-error-response (status-code method-not-allowed) req request-method)))))

(define (seval path req)
    (make-response
     (status-code ok)
     #f
     (time)
     "text/html"
     '()
     (make-reader-writer-body
      (lambda (iport oport options)
	(let ((sexp (read-request-sexp req iport)))
	  (http-syslog (syslog-level debug) "read sexp: ~a" sexp)
	  (with-tag oport HEAD ()
	    (newline oport)
	    (emit-title oport "Scheme program output"))
	  (newline oport)
	  
	  (with-tag oport BODY ()
	    (newline oport)
	    (do/timeout 
	     10
	     (receive vals
		 ;; Do the computation.
		 (begin (emit-header oport 2 "Output from execution")
			(newline oport)
			(with-tag oport PRE ()
			  (newline oport)
			  (force-output oport); In case we're gunned down.
			  (with-current-output-port oport
			      (eval-safely sexp))))
	       
	       ;; Pretty-print the returned value(s).
	       (emit-header oport 2 "Return value(s)")
	       (with-tag oport PRE ()
		 (for-each (lambda (val) (p val oport))
			   vals))))))))))


;;; Read an HTTP request entity body from stdin. The Content-length:
;;; element of request REQ's header tells how many bytes to this entity
;;; is. The entity should be a URI-encoded form body. Pull out the
;;;     program=<stuff>
;;; string, extract <stuff>, uri-decode it, parse that into an s-expression,
;;; and return it.

(define (read-request-sexp req iport)
  (cond 
   ((get-header (request-headers req) 'content-length) =>
    (lambda (cl-str)		 ; Take the first Content-length: header,
      (let* ((cl-start (string-skip cl-str char-set:whitespace))	   ; skip whitespace,
	     (cl (if cl-start			   	   ; & convert to
		     (string->number (substring cl-str   ;     a number.
						cl-start
						(string-length cl-str)))
		     0)) ; All whitespace?? -- WTF.
	     (qs (read-string cl iport))			; Read in CL chars,
	     (q (parse-html-form-query qs))		; and parse them up.
	     (s (cond ((assoc "program" q) => cdr)
		      (else (error "No program in entity body.")))))
	(http-syslog (syslog-level debug)
		     "Seval sexp: ~s" s)
	(read (make-string-input-port s)))))
   (else (error "No `Content-length:' field in POST request."))))

