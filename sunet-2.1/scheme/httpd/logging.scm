;;; logging.scm
;;; logging functionality for web server

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2002 by Martin Gasbichler.
;;; Copyright (c) 2002 by Andreas Bernauer.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define do-nothing-proc (lambda a #f))

(define-record-type logging :logging
  (really-make-logging log-port log-proc
		       syslog? syslog-proc
		       dns-lookup?)
  logging?
  ;; port to perform CLF-logging
  (log-port logging-log-port set-logging-log-port!)
  ;; proc to run for CLF-logging (req status-code)
  (log-proc logging-log-proc set-logging-log-proc!)
  ;; do syslogging?
  (syslog? logging-syslog? set-logging-syslog?!)
  ;; proc to run for syslog (level fmt . args)
  (syslog-proc logging-syslog-proc set-logging-syslog-proc!)
  ;; perform dns lookups?
  (dns-lookup? logging-dns-lookup? set-logging-dns-lookup?!))

(define (make-logging)
  (really-make-logging #f
		       do-nothing-proc
		       #f
		       do-nothing-proc
		       #f))

(define logging (make-preserved-thread-fluid #f))

(define (make-fluid-selector selector)
  (lambda () (selector (thread-fluid logging))))

(define (make-fluid-setter setter)
  (lambda (value)
    (setter (thread-fluid logging) value)))

(define logging-http-log-proc (make-fluid-selector logging-log-proc))
(define logging-http-syslog-proc (make-fluid-selector logging-syslog-proc))
(define logging-http-syslog? (make-fluid-selector logging-syslog?))
(define logging-http-log-port (make-fluid-selector logging-log-port))
(define logging-dns-lookup? (make-fluid-selector logging-dns-lookup?))

(define set-logging-http-log-proc (make-fluid-setter set-logging-log-proc!))
(define set-logging-http-syslog-proc (make-fluid-setter set-logging-syslog-proc!))
(define set-logging-http-syslog? (make-fluid-setter set-logging-syslog?!))
(define set-logging-http-log-port (make-fluid-setter set-logging-log-port!))
(define set-logging-dns-lookup? (make-fluid-setter set-logging-dns-lookup?!))

(define http-syslog
  (lambda a
    (apply (logging-http-syslog-proc) a)))

(define http-log
  (lambda a
    (apply (logging-http-log-proc) a)))

(define (http-syslog?)
  (logging-http-syslog?))

(define (init-http-log! options)
  ;; syslog has to be initialized before CLF-logging
  ;; because the latter may generate syslog-messages
  (init-http-syslog! (httpd-options-syslog? options))
  (init-http-port-log! (httpd-options-log-file options))
  (if (httpd-options-resolve-ips? options)
      (set-logging-dns-lookup? #t)
      (set-logging-dns-lookup? #f)))

(define (init-http-syslog! syslog?)
  (if syslog?
      (let ((http-syslog-lock (make-lock)))
	(set-logging-http-syslog? #t)
	(set-logging-http-syslog-proc
	      (lambda (level fmt . args)
		(with-lock http-syslog-lock
		  (lambda ()
		    (syslog level
			    (apply format #f fmt args)))))))
      (begin
	(set-logging-http-syslog? #f)
	(set-logging-http-syslog-proc do-nothing-proc))))

(define (init-http-port-log! log-file)
  (let ((logport 
	 (cond
	  ((string? log-file)            ; try to open log-file for appending (output)
	   (open-log-file log-file))
	  ((output-port? log-file)       ; we were given an output port, so let's use it
	   log-file)
	  ((eq? log-file #f)             ; no logging demanded
	   #f)
	; unexpected value of log-file; 
	  (else
	   (http-syslog 
	    (syslog-level warning)
	    "[httpd] Warning: Log-File was not specified correctly (given: ~S).~% No CLF logging."
	    log-file)
	   (make-null-output-port)))))

    (if log-file                         ; if logging was specified, set up the logger
	(let ((http-log-lock (make-lock)))
	  (set-logging-http-log-port logport)
	  (if (string? log-file)
	      (spawn (make-log-file-rotator log-file http-log-lock)))
	  (set-logging-http-log-proc (make-http-log-proc http-log-lock))))))
	
(define (make-http-log-proc http-log-lock)
  (lambda (req status-code)
    (if req
	(with-lock http-log-lock
	  (lambda ()
	    (display (make-CLF
		      (receive (host-address _) 
			  (socket-address->internet-address 
			   (socket-remote-address (request-socket req)))
			(format-internet-host-address host-address))
		      (request-method req) ; request method
		      (uri-path->uri
		       (http-url-path (request-url req))) ; requested file
		      (version->string (request-version req)) ; protocol version
		      (status-code-number status-code)
		      23		; filesize (unknown)
		      (get-header (request-headers req) 'referer)
		      (get-header (request-headers req) 'user-agent))
		     (logging-http-log-port))
	    (force-output (logging-http-log-port)))))))

(define (get-header headers tag)
  (cond
   ((assq tag headers) => cdr)
   (else #f)))

;; does the log-file rotation on signal USR1
(define (make-log-file-rotator log-file http-log-lock)
  (set-interrupt-handler interrupt/usr1 #f)
  (lambda ()
    (on-interrupt
     interrupt/usr1
     (lambda ()
       (with-lock http-log-lock
	 (lambda ()
	   (close-output-port (logging-http-log-port))
	   (set-logging-http-log-port (open-log-file log-file))))))))

(define (open-log-file log-file)	      
  (with-errno-handler*
   (lambda (errno packet)
     (http-syslog (syslog-level warning)
		  "[httpd] Warning: An error occured while opening ~S for writing (~A).~%Send signal USR1 when the problem is fixed.~%"
	     log-file
	     (car packet))
     (make-null-output-port))
   (lambda ()
     (open-output-file log-file 
		       (bitwise-ior open/create open/append)))))

; returns a string for a CLF entry (Common Log Format)
; note: till now, we do not log the user's time zone code
(define (make-CLF remote-ip request-type requested-file protocol http-code filesize referer user-agent)
  (format #f "~A - - ~A ~S ~A ~A ~S ~S~%"
		(or (maybe-dns-lookup remote-ip) "-")
		(format-date "[~d/~b/~Y:~H:~M:~S +0000]" (date))     ; +0000 as we don't know
		(string-join (list request-type 
				   (string-append "/" requested-file) 
				   protocol))
		; Unfortunately, we first split the request line into 
		; method/request-type etc. and put it together here.
		; Files conform to CLF are expected to print the original line.
		(or http-code "-")
		(or filesize "-")
		(if (string? referer) (string-trim referer) '-)
		(if (string? user-agent) 
		    (string-trim user-agent char-set:whitespace) 
		    '-)))


(define (maybe-dns-lookup remote-ip)
  (if (logging-dns-lookup?)
      (or (with-fatal-error-handler*
	   (lambda (condition decline)
	     (http-syslog (syslog-level debug)
			  "An error occured while resolving IP ~A: ~A"
			  remote-ip condition)
	     remote-ip)
	   (lambda ()
	     (dns-lookup-ip remote-ip)))
	  remote-ip)
      remote-ip))