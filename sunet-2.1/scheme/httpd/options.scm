;;; http server in the Scheme Shell	-*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2002 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; This package manages options to the http server as an abstract
;;; data type.

(define-record-type httpd-options :httpd-options
  (really-make-httpd-options port
			     root-directory
			     fqdn
			     reported-port
			     request-handler
			     server-admin
			     simultaneous-requests
			     log-file
			     syslog?
			     resolve-ips?
			     post-bind-thunk)
  httpd-options?
  (port httpd-options-port
	set-httpd-options-port!)
  (root-directory httpd-options-root-directory
		  set-httpd-options-root-directory!)
  (fqdn httpd-options-fqdn
	set-httpd-options-fqdn!)
  (reported-port httpd-options-reported-port
		 set-httpd-options-reported-port!)
  (request-handler httpd-options-request-handler
		   set-httpd-options-request-handler!)
  (server-admin httpd-options-server-admin
		set-httpd-options-server-admin!)
  (simultaneous-requests httpd-options-simultaneous-requests
			 set-httpd-options-simultaneous-requests!)
  (log-file httpd-options-log-file set-httpd-options-log-file!)
  (syslog? httpd-options-syslog? set-httpd-options-syslog?!)
  (resolve-ips? httpd-options-resolve-ips? set-httpd-options-resolve-ips?!)
  (post-bind-thunk httpd-options-post-bind-thunk set-httpd-options-post-bind-thunk!))

; default httpd-options generation
(define (make-default-httpd-options)
  (really-make-httpd-options 80		; port
			     "/"	; root-directory
			     #f		; fqdn
			     #f		; reported-port
			     #f		; request-handler
			     #f         ; server-admin
			     #f	        ; simultaneous-requests
			     #f
					; string: filename of log-file (directory must exist)
					; output-port: log to this port (e.g. (current-error-port))
					; #f: no logging
			     #t		; Do syslogging?
			     #t 	; Write host names instead of IPs in log-files?
			     #f))       ; post-bind-thunk

; creates a copy of a given httpd-option

(define (copy-httpd-options options)
  (let ((new-options (make-default-httpd-options)))
    (set-httpd-options-port! new-options
			     (httpd-options-port options))
    (set-httpd-options-root-directory! new-options
				       (httpd-options-root-directory options))
    (set-httpd-options-fqdn! new-options
			     (httpd-options-fqdn options))
    (set-httpd-options-reported-port! new-options
				      (httpd-options-reported-port options))
    (set-httpd-options-request-handler! new-options
					(httpd-options-request-handler options))
    (set-httpd-options-server-admin! new-options
				     (httpd-options-server-admin options))
    (set-httpd-options-simultaneous-requests!
     new-options
     (httpd-options-simultaneous-requests options))
    (set-httpd-options-log-file! new-options (httpd-options-log-file options))
    (set-httpd-options-syslog?! new-options (httpd-options-syslog? options))
    (set-httpd-options-resolve-ips?! new-options (httpd-options-resolve-ips? options))
    (set-httpd-options-post-bind-thunk! new-options 
					(httpd-options-post-bind-thunk options))
    new-options))

; (make-httpd-options-transformer set-option!) -> lambda (new-value [httpd-option])
; creates a transformer for httpd-options
; the returned procedure is called with the new value for the option
; and optionally with the httpd-option to change
(define (make-httpd-options-transformer set-option!)
  (lambda (new-value . stuff)
    (let ((new-options (if (not (null? stuff))
			   (copy-httpd-options (car stuff))
			   (make-default-httpd-options))))
      (set-option! new-options new-value)
      new-options)))

; several transformers for port, root-directory, etc.
(define with-port
  (make-httpd-options-transformer set-httpd-options-port!))
(define with-root-directory
  (make-httpd-options-transformer set-httpd-options-root-directory!))
(define with-fqdn
  (make-httpd-options-transformer set-httpd-options-fqdn!))
(define with-reported-port
  (make-httpd-options-transformer set-httpd-options-reported-port!))
(define with-request-handler
  (make-httpd-options-transformer set-httpd-options-request-handler!))
(define with-server-admin
  (make-httpd-options-transformer set-httpd-options-server-admin!))
(define with-simultaneous-requests
  (make-httpd-options-transformer set-httpd-options-simultaneous-requests!))
(define with-log-file
  (make-httpd-options-transformer set-httpd-options-log-file!))
(define with-syslog?
  (make-httpd-options-transformer set-httpd-options-syslog?!))
(define with-resolve-ips?
  (make-httpd-options-transformer set-httpd-options-resolve-ips?!))
(define with-post-bind-thunk
  (make-httpd-options-transformer set-httpd-options-post-bind-thunk!))

(define (make-httpd-options . stuff)
  (let loop ((options (make-default-httpd-options))
	     (stuff stuff))
    (if (null? stuff)
	options
	(let* ((transformer (car stuff))
	       (value (cadr stuff)))
	  (loop (transformer value options)
		(cddr stuff))))))
