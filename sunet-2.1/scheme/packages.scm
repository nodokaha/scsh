;; Scheme 48 package definitions for the
;; Scheme Untergrund Networking Suite

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; Copyright (c) 1996-2002 by Mike Sperber.
;;; Copyright (c) 2000-2002 by Martin Gasbichler.
;;; Copyright (c) 1998-2001 by Eric Marsden.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;; Interfaces

;; Net protocols and formats
 
(define-interface parse-html-forms-interface
  (export parse-html-form-query unescape-uri+))

(define-interface htmlout-interface
  (export emit-tag
	  emit-close-tag

	  emit-p
	  emit-title	
	  emit-header	; And so forth...

	  with-tag
	  with-tag*

	  escape-html
	  emit-text))

(define-interface smtp-interface
  (export smtp-send-mail
	  smtp-expand smtp-verify smtp-help
	  smtp-transactions
	  smtp-transactions/no-close
	  smtp-connect
	  smtp-helo smtp-mail smtp-rcpt smtp-data
	  smtp-send smtp-soml smtp-saml smtp-rset smtp-expn
	  smtp-help smtp-noop smtp-quit smtp-turn))

(define-interface rfc822-interface
  (export read-rfc822-headers
	  read-rfc822-headers-with-line-breaks
	  read-rfc822-field
	  read-rfc822-field-with-line-breaks
	  rfc822-time->string))

(define-interface uri-interface 
  (export parse-uri
	  uri-escaped-chars
	  unescape-uri
	  escape-uri
	  split-uri
	  uri-path->uri
	  simplify-uri-path))

(define-interface url-interface
  (export server?
	  make-server

	  server-user
	  server-password
	  server-host
	  server-port

	  parse-server
	  server->string

	  http-url?
	  make-http-url

	  http-url-server
	  http-url-path
	  http-url-search
	  http-url-fragment-identifier

	  parse-http-url
	  parse-http-url-string
	  http-url->string))

(define-interface ftp-library-interface
  (export copy-port->port-binary
	  copy-port->port-ascii
	  copy-ascii-port->port
	  parse-port-arg))

(define-interface ftp-interface
  (export ftp-connect
	  (ftp-type :syntax)
          ftp-set-type!
          ftp-rename
          ftp-delete
          ftp-cd
          ftp-cdup
          ftp-pwd
          ftp-rmdir
          ftp-mkdir
          ftp-modification-time
          ftp-size
          ftp-abort
          ftp-quit
          ftp-ls
          ftp-dir          
          ftp-get
          ftp-put
          ftp-append
          ftp-quot
	  ftp-error?

	  copy-port->port-binary
	  copy-port->port-ascii
	  copy-ascii-port->port))

(define-interface netrc-interface 
  (export netrc-machine-entry
	  netrc-entry?
	  netrc-entry-machine
	  netrc-entry-login
	  netrc-entry-password
	  netrc-entry-account
	  netrc-macro-definitions))

(define-interface pop3-interface
  (export pop3-connect
          pop3-stat
          pop3-retrieve-message
          pop3-retrieve-headers
          pop3-last
          pop3-delete
          pop3-reset
          pop3-quit
	  pop3-error?))

(define-interface rfc868-interface
  (export rfc868-time/tcp rfc868-time/udp))

(define-interface rfc867-interface
  (export rfc867-daytime/tcp rfc867-daytime/udp))

(define-interface dns-interface
  (export dns-clear-cache!		; clears the cache
	  dns-lookup			; complex lookup function
	  dns-lookup-name		; simple lookup function
	  dns-inverse-lookup		; obsolete, use dns-lookup-ip
	  dns-lookup-ip			; simple lookup function
	  dns-lookup-nameserver		; simple lookup function
	  dns-lookup-mail-exchanger	; simple lookpu function
	  pretty-print-dns-message	; prints a human readable dns-msg
	  force-ip			; reruns a lookup until a ip is resolved
	  force-ip-list			; reruns a lookup until a list of ips is resolved
	  address32->ip-string		; converts a address32 in an ip-string
	  ip-string->address32		; converts a ip-string in an address32
	  dns-find-nameserver		; returns a nameserver
	  dns-find-nameserver-list	; returns a list of nameservers
	  dns-check-nameservers         ; checks for working nameservers
	  socket-address->fqdn
	  host-fqdn
	  system-fqdn

          dns-get-information
          
          (network-protocol :syntax)
          network-protocol?
          dns-lookup
          
          dns-message? dns-message-query dns-message-reply dns-message-cache?
          dns-message-protocol dns-message-tried-nameservers

          pretty-print-dns-message
          
          message? message-header message-questions message-answers
          message-nameservers message-additionals message-source

          make-query-message make-simple-query-message
          
          header? header-flags header-question-count header-answer-count
          header-nameserver-count header-additional-count

          flags? flags-query-type flags-opcode flags-authoritative?
          flags-truncated? flags-recursion-desired? flags-recursion-available?
          flags-zero flags-response-code

          question? question-name question-type question-class

          (message-class :syntax)
          message-class? message-class-name message-class-number

          (message-type :syntax)
          message-type? message-type-name message-type-number

          resource-record?
          resource-record-name resource-record-type
          resource-record-class resource-record-ttl 
          resource-record-data
          
          resource-record-data-a? resource-record-data-a-ip
          resource-record-data-ns? resource-record-data-ns-name
          resource-record-data-cname? resource-record-data-cname-name
          resource-record-data-mx? resource-record-data-mx-preference
          resource-record-data-mx-exchanger resource-record-data-ptr?
          resource-record-data-ptr-name

          resource-record-data-soa?
          resource-record-data-soa-mname
          resource-record-data-soa-rname
          resource-record-data-soa-serial
          resource-record-data-soa-refresh
          resource-record-data-soa-retry
          resource-record-data-soa-expire
          resource-record-data-soa-minimum

          cache? cache-answer cache-ttl cache-time

          resolv.conf-parse-error?
          resolv.conf parse-resolv.conf! 
          
          domains-for-search))

(define-interface ips-interface
  (export address32->ip-string
	  ip-string->address32
	  ip-string->in-addr.arpa-string
	  octet-ip->address32 ;for dns.scm
	  ip-string?))

(define-interface cgi-scripts-interface
  (export cgi-form-query))

;; Utility libraries

(define-interface rate-limit-interface
  (export make-rate-limiter
	  rate-limit-block
	  rate-limit-open
	  rate-limit-close
	  rate-limiter-current-requests))

(define-interface crlf-io-interface 
  (export read-crlf-line
	  read-crlf-line-timeout
	  write-crlf))

(define-interface ls-interface 
  (export ls-crlf?
	  ls
	  arguments->ls-flags))

(define-interface format-net-interface
  (export format-internet-host-address
	  format-port))

(define-interface sunet-utilities-interface
  (export host-name-or-ip
	  on-interrupt
	  socket-address->string
	  dump
	  copy-inport->outport
	  dotdot-check
	  with-lock
	  get-header))

(define-interface handle-fatal-error-interface
  (export with-fatal-error-handler*
	  (with-fatal-error-handler :syntax)))

;; FTP server

(define-interface ftpd-interface 
  (export with-port with-anonymous-home with-banner with-log-port with-dns-lookup?
	  make-ftpd-options
	  ftpd
	  ftpd-inetd))

;; Web server

(define-interface httpd-core-interface
  (export httpd))

(define-interface httpd-make-options-interface
  (export make-httpd-options
	  with-port
	  with-root-directory
	  with-fqdn
	  with-reported-port
	  with-request-handler
	  with-server-admin
	  with-simultaneous-requests
	  with-log-file
	  with-syslog?
	  with-resolve-ips?
	  with-post-bind-thunk))

(define-interface httpd-read-options-interface
  (export httpd-options-port
	  httpd-options-root-directory
	  httpd-options-fqdn
	  httpd-options-reported-port
	  httpd-options-request-handler
	  httpd-options-server-admin
	  httpd-options-simultaneous-requests
	  httpd-options-log-file
	  httpd-options-syslog?
	  httpd-options-resolve-ips?
	  httpd-options-post-bind-thunk))

(define-interface httpd-access-control-interface
  (export access-denier
	  access-allower
	  access-controller
	  access-controlled-handler))

(define-interface httpd-errors-interface
  (export http-error?
	  http-error
	  fatal-syntax-error?
	  fatal-syntax-error))

(define-interface httpd-logging-interface
  (export init-http-log!
	  http-syslog?
	  http-syslog
	  http-log
	  logging
	  make-logging))

(define-interface httpd-requests-interface
  (export make-request			; HTTP request
	  request?			; record type.
	  request-method
	  request-uri
	  request-url
	  request-version
	  request-headers
	  request-socket

	  version< version<=
	  v0.9-request?
	  version->string))

(define-interface httpd-responses-interface
  (export make-response response?
	  response-code
	  response-message
	  response-seconds
	  response-mime
	  response-extras
	  response-body

	  make-nph-response nph-response?
	  nph-response-body

	  make-input-response input-response?
	  input-response-body-maker

	  make-writer-body writer-body?
	  make-reader-writer-body reader-writer-body?
	  make-redirect-body redirect-body? redirect-body-location
	  display-http-body

	  status-code?
	  status-code-number
	  status-code-message
	  (status-code :syntax)
	  name->status-code
	  number->status-code

	  make-error-response
	  make-redirect-response))

(define-interface httpd-basic-handlers-interface
  (export make-predicate-handler
	  make-path-predicate-handler
	  make-host-name-handler
	  make-path-prefix-handler
	  alist-path-dispatcher
	  null-request-handler))

(define-interface httpd-file-directory-handlers-interface
  (export home-dir-handler
	  tilde-home-dir-handler
	  rooted-file-handler
	  rooted-file-or-directory-handler
	  
	  make-file-directory-options
	  with-file-name->content-type
	  with-file-name->content-encoding
	  with-file-name->icon-url
	  with-blank-icon-url
	  with-back-icon-url
	  with-unknown-icon-url))

(define-interface httpd-seval-handlers-interface
  (export seval-handler))

(define-interface httpd-info-gateway-interface
  (export info-handler
	  find-info-file
	  info-gateway-error))

(define-interface httpd-rman-gateway-interface
(export rman-handler
	man
	parse-man-entry
	cat-man-page
	find-man-file
	file->man-directory
	cat-n-decode
	nroff-n-decode))

(define-interface httpd-cgi-handlers-interface 
  (export cgi-default-bin-path
	  cgi-handler))

(define-interface loser-interface (export loser))

(define-interface toothless-interface (interface-of scheme))

(define-interface toothless-eval-interface (export eval-safely))

;; Structures

(define-structure sunet-version (export sunet-version-identifier)
  (open scheme)
  (begin
    (define sunet-version-identifier "2.1")))

;; Net protocols and formats

(define-structure parse-html-forms parse-html-forms-interface
  (open scheme-with-scsh 
	let-opt 
	(subset srfi-13 (string-index string-map))
	receiving 
	uri)
  (files (lib parse-forms)))

(define-structure htmlout htmlout-interface
  (open scheme-with-scsh
	(subset srfi-13 (string-fold))
	formats
	ascii
	receiving)
  (files (lib htmlout)))

(define-structure smtp smtp-interface
  (open scheme-with-scsh
	signals conditions
	define-record-types
	(subset srfi-1 (filter-map))
	(subset srfi-13 (string-tokenize string-join))
	crlf-io				; read-crlf-line write-crlf
	receiving			; values receive
	dns				; SYSTEM-FQDN
	let-opt
	(subset rfc822 (rfc822-time->string)))
  (files (lib smtp)))

(define-structure rfc822 rfc822-interface
  (open scheme-with-scsh
	receiving
	(subset srfi-13 (string-map string-index string-concatenate))
	let-opt
	crlf-io
	ascii)
  (files (lib rfc822)))

(define-structure uri uri-interface
  (open scheme-with-scsh
	(subset srfi-13 (string-index string-index-right string-fold string-join))
	let-opt
	receiving
	ascii
	bitwise
	field-reader-package)
  (files (lib uri)))

(define-structure url url-interface
  (open scheme-with-scsh
	define-record-types
	receiving
	(subset srfi-13 (string-index))
	uri
	httpd-errors)
  (files (lib url)))

(define-structure ftp-library ftp-library-interface
  (open scheme-with-scsh
	(subset signals (call-error))
	(subset srfi-1 (any))
	crlf-io)
  (files (lib ftp-library)))

(define-structure ftp ftp-interface
  (open scheme-with-scsh
	netrc
        define-record-types
	finite-types
        receiving
        handle
        conditions
        signals
	(subset srfi-13 (string-join string-prefix?))
	let-opt
	sunet-utilities
	format-net
	crlf-io
	ftp-library)
  (files (lib ftp)))

(define-structure netrc netrc-interface
  (open scheme-with-scsh
	define-record-types
	srfi-14)
  (files (lib netrc)))

(define-structure pop3 pop3-interface
  (open scheme-with-scsh
	netrc rfc822
        define-record-types
        handle
        conditions handle-fatal-error
        signals
	(subset srfi-13 (string-index string-prefix? string-join))
	let-opt
	crlf-io)
  (files (lib pop3)))

(define-structures ((rfc867 rfc867-interface)
		    (rfc868 rfc868-interface))
  (open scheme-with-scsh
	handle-fatal-error)
  (files (lib nettime)))

(define-structure dns dns-interface
  (open scheme-with-scsh
	(subset srfi-1 (filter reverse! delete lset-difference lset-union))
	tables
	ascii
	formats
	signals
	finite-types
	define-record-types
	random
	queues
	conditions
	handle
	sort
	threads
	locks
	ips)
  (files (lib dns)))

(define-structure ips ips-interface
  (open scheme-with-scsh
	formats)
  (files (lib ip)))

(define-structure cgi-scripts cgi-scripts-interface
  (open scheme-with-scsh
	parse-html-forms)
  (files (lib cgi-script)))

;; Utility libraries

(define-structure rate-limit rate-limit-interface
  (open scheme
	define-record-types
	locks
	signals)
  (files (lib rate-limit)))

(define-structure crlf-io crlf-io-interface
  (open scheme-with-scsh
	ascii		; ascii->char
	receiving	; MV return (RECEIVE and VALUES)
	let-opt		; let-optionals
	threads         ; sleep
	)	
  (files (lib crlf-io)))

(define-structure ls ls-interface
  (open scheme-with-scsh
	handle
	(subset srfi-1 (filter))
	bitwise
	fluids
	crlf-io)
  (files (lib ls)))

(define-structure format-net format-net-interface
  (open scheme-with-scsh
	let-opt)
  (files (lib format-net)))

(define-structure sunet-utilities sunet-utilities-interface
  (open scheme-with-scsh
	format-net
	sigevents
	(subset srfi-13 (string-join))
	dns
	let-opt				; :optional
	locks
	handle-fatal-error)
  (files (lib sunet-utilities)))

(define-structure handle-fatal-error handle-fatal-error-interface
  (open scheme conditions handle)
  (files (lib handle-fatal-error)))

;; FTP server

(define-structure ftpd ftpd-interface 
  (open scheme-with-scsh
	conditions handle signals
	define-record-types
	handle-fatal-error
	threads threads-internal    ; last one to get CURRENT-THREAD
	fluids thread-fluids
	locks
	(subset srfi-13 (string-map string-trim-both string-index))
	(subset srfi-1 (partition))
	crlf-io
	ls
	ftp-library
	dns
	sunet-version
	sunet-utilities
	receiving
	format-net)
  (files (ftpd ftpd)))

;; Web server

(define-structure httpd-core httpd-core-interface
  (open scheme-with-scsh
	thread-fluids			; fork-thread
	receiving
	crlf-io				; write-crlf, read-crlf-line
	rfc822
	handle				; ignore-errors
	conditions			; condition-stuff
	uri
	url
	format-net
	rate-limit			; rate-limiting stuff
	(subset srfi-13 (string-index))
	dns				; dns-lookup-ip
	sunet-utilities                 ; socket-address->string 
	locks				; make-lock et al.
	fluids				; let-fluid
	enumerated			; enum
	architecture			; os-error

	handle-fatal-error
	httpd-read-options
	httpd-errors
	httpd-logging
	httpd-requests
	httpd-responses

	sunet-version
	)
  (files (httpd core)))

(define-structures ((httpd-make-options httpd-make-options-interface)
		    (httpd-read-options httpd-read-options-interface))
  (open scheme
	define-record-types)
  (files (httpd options)))

(define-structure httpd (compound-interface httpd-core-interface
					    httpd-make-options-interface)
  (open httpd-core
	httpd-make-options))

(define-structure httpd-access-control httpd-access-control-interface
  (open scheme-with-scsh
	(subset srfi-1 (any every))
	httpd-responses
	httpd-requests
	httpd-errors
	(subset srfi-13 (string-map))
	)
  (files (httpd access-control)))

(define-structure httpd-errors httpd-errors-interface
  (open conditions signals handle scheme)
  (files (httpd error)))

(define-structure httpd-logging httpd-logging-interface
  (open scheme-with-scsh
	httpd-read-options
	i/o				; make-null-output-port
	locks
	receiving
	uri				; uri-path->uri
	url				; http-url-path
	httpd-requests			; request record
	httpd-responses
	formats
	format-net			; format-internet-host-address
	(subset srfi-13 (string-join string-trim))
	rfc822				; get-header
	sunet-utilities			; on-interrupt
	threads				; spawn
	dns				; dns-lookup-ip
	define-record-types
	thread-fluids			; make-preserved-fluid et al.
	handle-fatal-error
	)
  (files (httpd logging)))

(define-structure httpd-requests httpd-requests-interface
  (open scheme
	define-record-types)
  (files (httpd request)))

(define-structure httpd-responses httpd-responses-interface
  (open scheme
	(subset scsh (format-date write-string time date))
	syslog
	define-record-types
	finite-types
	formats
	(subset signals (call-error))
	httpd-requests
	httpd-read-options)
  (files (httpd response)))

(define-structure httpd-basic-handlers httpd-basic-handlers-interface
  (open scheme-with-scsh
	rfc822
	httpd-requests			; REQUEST record type, v0.9-request
	(subset srfi-1 (fold-right))
	(subset srfi-13 (string-trim string-prefix-ci?))
	httpd-responses
	httpd-errors
	)
  (files (httpd handlers)))

(define-structure httpd-file-directory-handlers httpd-file-directory-handlers-interface
  (open scheme-with-scsh
	define-record-types
	httpd-core
	httpd-requests
	httpd-responses
	httpd-errors
	httpd-basic-handlers
	httpd-read-options
	url
	htmlout
	crlf-io
	(subset srfi-13 (string-join))
	(subset rfc822 (rfc822-time->string))
	sunet-utilities			; dotdot-check, copy-inport->outport
	conditions
	let-opt
	handle-fatal-error
	)
  (files (httpd file-dir-handler)))

(define-structure httpd-seval-handlers httpd-seval-handlers-interface
  (open scheme-with-scsh		; syscalls & INDEX
	httpd-errors
	httpd-requests			; v0.9-request
	httpd-responses
	httpd-logging			; http-log
	uri				; UNESCAPE-URI
	htmlout				; Formatted HTML output
	pp
	(subset srfi-13 (string-skip))
	rfc822
	toothless-eval			; EVAL-SAFELY
	handle				; IGNORE-ERROR
	parse-html-forms		; PARSE-HTML-FORM-QUERY
	threads				; SLEEP
	sunet-utilities			; GET-HEADER
	)
  (files (httpd seval)))

(define-structure httpd-info-gateway httpd-info-gateway-interface
  (open scheme-with-scsh
	(subset srfi-1 (find))
	(subset srfi-13 (string-map string-skip string-index))
	conditions signals handle
	htmlout
	httpd-requests
	httpd-responses
	httpd-errors
	url
	uri
	handle-fatal-error)
  (files (httpd info-gateway)))

(define-structure httpd-rman-gateway httpd-rman-gateway-interface
  (open scheme-with-scsh
	httpd-responses
	httpd-requests
	httpd-errors
	conditions
	url
	uri
	htmlout
	httpd-basic-handlers
	handle-fatal-error
	let-opt
	sunet-utilities
	(subset srfi-13 (string-join))
	)
  (files (httpd rman-gateway)))

(define-structure httpd-cgi-handlers httpd-cgi-handlers-interface
  (open	scheme-with-scsh
	(subset srfi-1 (alist-delete))
	(subset srfi-13 (string-prefix? string-index string-trim substring/shared))
	rfc822
	crlf-io			; WRITE-CRLF
	uri
	url			; HTTP-URL record type
	httpd-logging
	httpd-requests
	httpd-responses
	httpd-basic-handlers	; HTTP-HOMEDIR, SERVE-ROOTED-FILE-PATH
	httpd-errors		; HTTP-ERROR
	httpd-file-directory-handlers		; dot-dot-check, copy-inport->outport
	sunet-version
	formats
	format-net
	sunet-utilities         ; host-name-or-empty, get-header
	let-opt                 ; let-optionals
	handle-fatal-error
	)
  (files (httpd cgi-server)))

(define-structure loser (export loser)
  (open scheme signals)
  (begin (define (loser name)
	   (lambda x (error "Illegal call" name)))))

(define-structure toothless toothless-interface
  (open scheme loser)
  (begin
    (define call-with-input-file	(loser "call-with-input-file"))
    (define call-with-output-file	(loser "call-with-output-file"))
    (define load			(loser "load"))
    (define open-input-file		(loser "open-input-file"))
    (define open-output-file		(loser "open-output-file"))
    (define transcript-on		(loser "transcript-on"))
    (define with-input-from-file	(loser "with-input-from-file"))
    (define with-input-to-file		(loser "with-input-to-file"))
    (define eval			(loser "eval"))
    (define interaction-environment	(loser "interaction-environment"))
    (define scheme-report-environment	(loser "scheme-report-environment"))))

(define-structure toothless-eval toothless-eval-interface
  (open scheme
	package-commands-internal	; config-package, get-reflective-tower
	packages			; structure-package, make-simple-package
	environments			; environment-ref
	handle				; ignore-errors
	)
  (access toothless)	; Force it to be loaded.
  (begin

    (define toothless-struct (environment-ref (config-package) 'toothless))
    (define toothless-package (structure-package toothless-struct))

    (define (new-safe-package)
      (make-simple-package (list toothless-struct) #t
			   (get-reflective-tower toothless-package) ; ???
			   'safe-env))

    (define (eval-safely exp)
      (ignore-errors (lambda () (eval exp (new-safe-package)))))))
