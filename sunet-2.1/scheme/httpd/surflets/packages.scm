;; Structures and interfaces for surflets.

;;; Copyright 2002, 2003 Andreas Bernauer
;;; Copyright 2002 Martin Gasbichler

;;; NOTE: SSAX/lib/packages.scm must be loaded before you can use this
;;; downloadable from
;;; http://sourceforge.net/project/showfiles.php?group_id=30687 (take
;;; the r5rs compliant version (ssax-sr5rs-plt200-4.9.tar.gz); and
;;; patch string->goodhtml in packages.scm to string->goodHTML)

;;; Interfaces
;; Surflet Handler
(define-interface surflet-handler-interface
  (export surflet-handler))

;; Responses from SUrflets
(define-interface surflet-handler/responses-interface
  (export make-surflet-response
	  valid-surflet-response-data?
	  surflet-response?
	  surflet-response-status
	  surflet-response-content-type
	  surflet-response-headers
	  surflet-response-data))

;; SUrflet-requests as expected from the surflet handler
(define-interface surflet-handler/requests-interface
  (export make-surflet-request		;FIXME? unusable for user
	  surflet-request?
	  surflet-request-request
	  surflet-request-input-port
	  surflet-request-method
	  surflet-request-uri
	  surflet-request-url
	  surflet-request-version
	  surflet-request-headers
	  surflet-request-socket))

(define-interface surflets/error-interface
  (export send-error			;send error response
	  (status-code :syntax)))	;from httpd-responses

;; Use for SUrflets
(define-interface surflet-handler/primitives-interface
  (compound-interface 
   surflet-handler/responses-interface
   surflet-handler/requests-interface
   surflets/error-interface
   (export send/suspend			;send and suspend
	   send/finish			;send and finish
	   send				;just send (no finish, no suspend)
	   )))


;; Send HTML-Strings (for advanced user)
(define-interface surflets/send-html-string-interface
  (export send-html-string/suspend
	  send-html-string/finish
	  send-html-string))

;; Extensions/Exports to/from Olegs SSAX library
(define-interface surflets/sxml-interface
  (export display-low-level-sxml
	  sxml->low-level-sxml		;direct map to pre-post-order
	  sxml->string
	  sxml->string/internal
	  sxml-attribute?
	  sxml-attribute-attributes
	  default-rule
	  text-rule
	  attribute-rule))

;; SUrflets' extensions to SXML
(define-interface surflets/surflet-sxml-interface
  (export surflet-sxml->low-level-sxml
	  surflet-sxml-rules
	  surflet-form-rule
	  default-rules
	  plain-html-rule
	  nbsp-rule
	  url-rule))

;; Use for advanced users: make your own conversion rules.
(define-interface surflets/my-sxml-interface
  (compound-interface
   surflets/send-html-string-interface
   surflets/sxml-interface
   surflets/surflet-sxml-interface))

(define-interface surflets/continuations-interface
  (export get-continuations
	  delete-continuation!
	  continuation-id))

;; Access to session-id and continuation-id
(define-interface surflets/ids-interface
  (export my-session-id
	  my-continuation-id
	  my-ids
	  instance-session-id))

(define-interface surflets/session-data-interface
  (export get-session-data
	  set-session-data!))

;; Use for advanced users: access to your sessions and continuations
;; (currently you get access to all sessions; this should and will be
;; restricted in the future)
(define-interface surflets/my-sessions-interface
  (compound-interface 
   surflets/ids-interface
   surflets/continuations-interface
   surflets/session-data-interface
   (export get-session
	   ;; That would be too much:
	   ;; get-sessions
	   delete-session!
	   instance-session-id
	   session-adjust-timeout!
	   adjust-timeout!
	   session-alive?
	   session-surflet-name
	   session-session-id
	   set-session-lifetime!
	   options-surflet-path
	   options-session-lifetime
	   options-cache-surflets?
           options-make-session-timeout-text)))

(define-interface surflets/sessions-interface
  (compound-interface
   surflets/session-data-interface
   (export get-session
	   get-sessions
	   delete-session!
	   instance-session-id
           set-session-lifetime!
	   adjust-timeout!
           session-adjust-timeout!
	   session-alive?
	   session-surflet-name
	   session-session-id		;faked
	   ;; FIXME: This is too much and should be restricted:
	   session-continuation-table
	   session-continuation-table-lock
	   session-continuation-counter)))

(define-interface surflet-handler/surflets-interface
  (export get-loaded-surflets
	  unload-surflet
	  reset-surflet-cache!))

(define-interface surflet-handler/options-interface
  (export make-surflet-options
	  with-surflet-path
          with-session-lifetime
          with-cache-surflets?
          with-make-session-timeout-text
	  options-surflet-path
          options-session-lifetime
	  options-cache-surflets?
          options-make-session-timeout-text
	  set-options-surflet-path!
	  set-options-session-lifetime!
	  set-options-cache-surflets?!
          set-options-make-session-timeout-text))

(define-interface surflet-handler/resume-url-interface
  (export resume-url?
	  resume-url-ids
	  resume-url-session-id
	  resume-url-continuation-id))

;; Use for adminstration of the Surflet Handler
(define-interface surflet-handler/admin-interface
  (compound-interface
   surflet-handler/surflets-interface
   surflets/sessions-interface
   surflets/continuations-interface
   surflet-handler/resume-url-interface
   surflet-handler/options-interface
))

;; THE interface that SUrflets use.
(define-interface surflet-interface
  (export main))			; MAIN gets one parameter, the REQUEST

;; Simple Surflet API as known from PLT
(define-interface simple-surflet-api-interface
  (export single-query
	  queries
	  form-query
	  inform
	  final-page
	  make-text
	  make-password
	  make-number
	  make-boolean
	  make-radio
	  make-yes-no
	  extract/single
	  extract))

;; shift-reset
(define-interface shift-reset-interface
  (export (reset :syntax)
	  (shift :syntax)))

;; For memory profiling
(define-interface profiling-interface
  (export profile-space
	  profile-result
	  profile-results
	  write-gnuplot-data-file

	  space-info-pair		space-info-symbol
	  space-info-vector		space-info-closure
	  space-info-location		space-info-cell
	  space-info-channel		space-info-port
	  space-info-ratnum		space-info-record
	  space-info-continuation	space-info-extended-number
	  space-info-template		space-info-weak-pointer
	  space-info-shared-binding	space-info-unused-d-header1
	  space-info-unused-d-header2	space-info-string
	  space-info-byte-vector	space-info-double
	  space-info-bignum		space-info-total

	  set-space-info-pair!			set-space-info-symbol!
	  set-space-info-vector!		set-space-info-closure!
	  set-space-info-location!		set-space-info-cell!
	  set-space-info-channel!		set-space-info-port!
	  set-space-info-ratnum!		set-space-info-record!
	  set-space-info-continuation!		set-space-info-extended-number!
	  set-space-info-template!		set-space-info-weak-pointer!
	  set-space-info-shared-binding!	set-space-info-unused-d-header1!
	  set-space-info-unused-d-header2!	set-space-info-string!
	  set-space-info-byte-vector!		set-space-info-double!
	  set-space-info-bignum!		set-space-info-total!

	  pure-count	  pure-bytes
	  impure-count	  impure-bytes
	  total-count	  total-bytes
	  ))

;; Handling every condition
(define-interface handle-fatal-interface
  (export with-fatal-handler*
	  (with-fatal-handler :syntax)))

;; Thread-safe counter
(define-interface thread-safe-counter-interface
  (export make-thread-safe-counter
	  thread-safe-counter-value
	  thread-safe-counter-next!
	  thread-safe-counter?))



;; These two are from Martin Gasbichler:
(define-interface rt-module-language-interface 
  (export ((lambda-interface 
	    with-names-from-rt-structure)
	   :syntax)
	  reify-structure
	  load-structure
	  load-config-file
	  rt-structure-binding))

(define-interface rt-modules-interface
  (export interface-value-names 
	  reify-structure
	  load-config-file
	  rt-structure-binding
	  load-structure))

(define-interface with-locks-interface
  (export with-lock*
	  (with-lock :syntax)))

;; Input-fields as Scheme-Objects
(define-interface surflets/input-field-value-interface
  (export input-field?
	  raw-input-field-value
	  input-field-value
	  input-field-binding))

;; For advanced users: creating your own input-fields
(define-interface surflets/my-input-fields-interface
  (compound-interface 
   surflets/input-field-value-interface
   (export generate-input-field-name
	   make-input-field
	   make-multi-input-field
	   input-field-name
	   input-field-type
	   input-field-transformer
	   input-field-attributes 
	   input-field-html-tree-maker
	   input-field-html-tree
	   input-field-multi?
	   set-input-field-attributes!
	   touch-input-field!)))

;; For internal use: special exports to create 
;; SXL-rules for input-fields
(define-interface surflets/internal-input-fields-interface
  (export *input-field-trigger*
	  make-sxml-input-field))

(define-interface surflets/surflet-input-fields-interface
  (compound-interface
   surflets/input-field-value-interface
   (export make-text-field
	   set-text-field-value!

	   make-number-field
	   set-number-field-value!

	   make-hidden-field
	   set-hidden-field-value!
	   
	   make-password-field
	   set-password-field-value!

	   make-textarea
	   set-textarea-value!

	   make-select
	   make-annotated-select
	   make-simple-select-option
	   make-annotated-select-option
	   select-option?
	   select-select-option!
	   unselect-select-option!
	   add-select-option!
	   delete-select-option!
	   set-select-option-selected?!
	   
	   make-radio-group
	   make-annotated-radio-group
	   make-radios
	   check-radio!
	   uncheck-radio!
	   set-radio-checked?!

	   make-checkbox
	   make-annotated-checkbox
	   check-checkbox!
	   uncheck-checkbox!
	   set-checkbox-checked?!

	   make-submit-button
	   make-reset-button
	   make-image-button)))

;; Some utilities
(define-interface surflets/utilities-interface
  (export form-query-list
	  rev-append
	  generate-unique-number
	  generate-unique-name
	  identity))

;; Intelligent Addresses
(define-interface surflets/addresses-interface
  (export make-address
	  make-annotated-address
	  address-name
	  address-annotated?
;	  address-add-annotation!
	  address-annotation))

(define-interface surflets/callbacks-interface
  (export make-callback
	  make-annotated-callback
	  callback-function))

;; Returned-via (dispatcher for input-fields and intelligent
;; addresses)
(define-interface surflets/returned-via-interface
  (export returned-via
	  returned-via?
	  (case-returned-via :syntax)))

;; Outdater denies access to outdated object
(define-interface surflets/outdaters-interface
  (export make-outdater
	  (if-outdated :syntax)
	  show-outdated))

;; Access to form bindings in URL
(define-interface surflets/bindings-interface
  (export get-bindings
	  get-content-length
	  extract-bindings
	  extract-single-binding))

;; HTML-Extensions to send/suspend et al. (for basic user)
(define-interface surflets/send-html-interface
  (export send-html/suspend
	  send-html/finish
	  send-html))



;; Helping functions for surflets (for basic user)
(define-interface surflets-interface
  (compound-interface
;   surflet-handler/surflets-interface;
;   surflets/sxml-interface
;   surflets/surflet-sxml-interface
   surflets/send-html-interface
   surflets/surflet-input-fields-interface
   surflets/addresses-interface
   surflets/returned-via-interface
   surflets/bindings-interface
   surflets/session-data-interface))

;;; Structures
;; structures from SUrflet Handler
(define-structures
  ((surflet-handler surflet-handler-interface)
   (surflet-handler/surflets surflet-handler/surflets-interface)
   (surflet-handler/options surflet-handler/options-interface)
   (surflet-handler/resume-url surflet-handler/resume-url-interface)
   (surflet-handler/admin surflet-handler/admin-interface)
   (surflet-handler/primitives surflet-handler/primitives-interface)
   (surflets/sessions surflets/sessions-interface)
   (surflets/continuations surflets/continuations-interface)
   (surflets/error surflets/error-interface)
   (surflets/session-data surflets/session-data-interface))
  (open	define-record-types		;DEFINE-RECORD-TYPE
	handle-fatal    		;WITH-FATAL-ERROR-HANDLER* et al.
	handle-fatal-error
	httpd-errors			;errors for httpd
	httpd-logging			;HTTP-SYSLOG
	httpd-requests			;requests from httpd
	httpd-responses			;replies for httpd 
	let-opt				;:OPTIONAL
	locks				;MAKE-LOCK et al.
	profiling			;PROFILE-SPACE
	rt-module-language		;get structures dynamically
	scheme-with-scsh		;regexp et al.
        search-trees
	shift-reset			;SHIFT and RESET
        (subset srfi-1 (alist-cons alist-delete!))
	srfi-6				;string-ports
	srfi-14				;CHAR-SET:DIGIT
	srfi-27				;random numbers
	surflet-requests		;requests for surflets
	surflet-responses		;responses from surflets
	sxml-to-html			;SXML->HTML
	tables				;HASH-TABLES
	thread-cells			;THREAD-CELL et al.
	thread-fluids			;FORK-THREAD
	thread-safe-counter
	threads				;SLEEP
	uri				;URI-PATH-LIST->PATH
	with-locks			;WITH-LOCK
	)
  (files surflet-handler))

;; SUrflets library of helping functions 
(define-structure surflets surflets-interface
  (open	surflets/session-data
	surflets/send-html		;send-html/suspend...
	surflets/surflet-input-fields
	surflets/addresses		;annotated-address...
	surflets/returned-via
	surflets/bindings))

;; SUrflets library for advanced users: make and use your own
;; conversion rules.
(define-structure surflets/my-sxml surflets/my-sxml-interface
  (open	surflets/send-html-string
	surflets/sxml
	surflets/surflet-sxml))

;; SUrflets librarary for advanced users: access to session and
;; continuations and stuff.
(define-structure surflets/my-sessions surflets/my-sessions-interface
  (open surflets/ids
	surflets/continuations
	surflets/session-data
	surflet-handler/surflets
	surflets/sessions
	surflet-handler/options))


;; Shift-Reset
(define-structure shift-reset shift-reset-interface
  (open scheme
	signals
	escapes
	thread-cells)
  (files shift-reset))

;; Measuring memory usage.
(define-structure profiling profiling-interface
  (open let-opt
	define-record-types
	spatial
	srfi-13
	srfi-1
	locks
	scheme-with-scsh)
  (files profile))

;; Simple Surflet API as known from PLT.
(define-structure simple-surflet-api simple-surflet-api-interface
  (open scheme-with-scsh
	define-record-types
	let-opt
	surflets
	surflets/surflet-input-fields
	(subset srfi-1 (zip filter find make-list))
	handle-fatal-error
	)
  (files simple-surflet-api))

;; Handling every condition
(define-structure handle-fatal handle-fatal-interface
  (open scheme conditions handle)
  (files handle-fatal))
	

;; Thread-safe counter
(define-structure thread-safe-counter thread-safe-counter-interface
  (open scheme 
	locks
	define-record-types)
  (files thread-safe-counter))

;; SUrflet-requests as expected from the SUrflet handler 
;;; We have two names for the same thing to ease the use of structure
;;; names: requests seem to be part of the surflet-handler, but are
;;; actually seperate files. If you know everything about SUrflets,
;;; you use `surflet-requests'.
(define-structures
  ((surflet-handler/requests surflet-handler/requests-interface)
   (surflet-requests surflet-handler/requests-interface))
  (open scheme
	define-record-types
	httpd-requests)
  (files surflet-request))

(define-structures
  ((surflet-handler/responses surflet-handler/responses-interface)
   (surflet-responses surflet-handler/responses-interface))
  (open scheme-with-scsh
	define-record-types)
  (files surflet-response))

;; Extensions to Olegs SSAX library
(define-structure surflets/sxml surflets/sxml-interface
  (open scheme-with-scsh		;string-ports
	(subset sxml-to-html (string->goodHTML entag))
	(subset sxml-tree-trans (pre-post-order)))
  (files sxml))


;; Input fields as Scheme objects
(define-structures 
  ((surflets/input-field-value surflets/input-field-value-interface)
   (surflets/my-input-fields surflets/my-input-fields-interface)
   (surflets/internal-input-fields surflets/internal-input-fields-interface))
  (open scheme-with-scsh		;error, format
	(subset let-opt (:optional))
	handle-fatal-error
	define-record-types
	surflets/sxml
	surflets/utilities
	)
  (files input-fields))

(define-structure surflets/input-fields surflets/my-input-fields)

(define-structure surflets/surflet-input-fields
  surflets/surflet-input-fields-interface
  (open scheme-with-scsh		;error, format
	;; avoid name collision for member
	(modify srfi-1 (rename (member member/srfi-1)))
	define-record-types
	let-opt
	surflets/my-input-fields
	surflets/utilities		;generate-unique-number
	surflets/sxml
	tables				;make-integer-table
	)
  (files surflet-input-fields))


;; Extensions to SXML for surflets
(define-structure surflets/surflet-sxml surflets/surflet-sxml-interface
  (open scheme-with-scsh		;error,receive
	(subset surflets/my-input-fields (input-field-html-tree))
	(subset surflets/internal-input-fields 
		(*input-field-trigger* 
		 make-sxml-input-field))
	surflets/utilities
	(subset srfi-1 (make-list))
	 surflets/sxml)
  (files surflet-sxml))


;; Access to session-id and continuation-id
(define-structure surflets/ids surflets/ids-interface
  (open scheme
	(subset surflet-requests (surflet-request-url))
	(subset srfi-1 (last))
	(subset surflet-handler/admin 
		(instance-session-id
		 resume-url-session-id
		 resume-url-continuation-id
		 resume-url-ids))
	(subset url (http-url-path)))
  (files ids))


;; Some utilities
(define-structure surflets/utilities surflets/utilities-interface
  (open scheme
	parse-html-forms)
  (files utilities))


;; Intelligent Addresses 
(define-structure surflets/addresses surflets/addresses-interface
  (open scheme
	srfi-23				;error
	(subset uri (escape-uri))
	define-record-types
	(subset surflets/utilities (generate-unique-name)))
  (files addresses))

(define-structure surflets/callbacks surflets/callbacks-interface
  (open scheme
	srfi-23				;error
	surflets/addresses
	(subset surflet-handler/primitives (send/suspend))
	surflets/bindings
	surflets/returned-via)
  (files callbacks))

(define-structure surflets/returned-via surflets/returned-via-interface
  (open scheme
	surflets/input-field-value
	surflets/addresses
	(subset uri (unescape-uri)))
  (files returned-via))

(define-structure surflets/outdaters surflets/outdaters-interface
  (open scheme
	define-record-types
	surflets/send-html)
  (files outdater))

(define-structure surflets/bindings surflets/bindings-interface
  (open scheme-with-scsh		;read-string,error
	locks
	weak				;weak pointers
	surflets/utilities		;form-query-list
	surflet-requests
	(subset url (http-url-search))
	(subset srfi-14 (char-set:digit))
	(subset srfi-13 (string-index string-trim))
	(subset srfi-1 (filter))
	(subset sunet-utilities (get-header)))
  (files bindings))

(define-structures
  ((surflets/send-html surflets/send-html-interface)
   (surflets/send-html-string surflets/send-html-string-interface))
  (open scheme
	surflet-handler/primitives
	surflets/sxml
	surflets/surflet-sxml)
  (files send-html))

;; These two are from Martin Gasbichler:
(define-structure rt-module-language rt-module-language-interface
  (open scheme
	rt-modules)
  (for-syntax (open scheme
		    rt-modules))
  (begin
    (define-syntax lambda-interface
      (lambda (expr rename compare)
	(let ((%lambda (rename 'lambda))
	      (interface-name (cadr expr))
	      (body (cddr expr)))
	  `(,%lambda ,(interface-value-names interface-name) ,@body))))

;(with-names-from-rt-structure surflet surflet-interface (main))
    (define-syntax with-names-from-rt-structure
      (lambda (expr rename compare)
	(let ((%lambda (rename 'lambda))
	      (%let (rename 'let))
	      (%rt-structure-value (rename 'rt-structure-value))
	      (%rt-structure-binding (rename 'rt-structure-binding))
	      (rt-structure (cadr expr))
	      (interface-name (caddr expr))
	      (body (cdddr expr)))
 	  (let ((ivn (interface-value-names interface-name)))
	    `(,%let ((,%rt-structure-value ,rt-structure))
	       ((,%lambda ,ivn ,@body)
		,@(map (lambda (name)
			 `(,%rt-structure-binding ,%rt-structure-value ',name))
		       ivn)))))))))

(define-structure rt-modules rt-modules-interface
  (open scheme
	meta-types ; syntax-type
	interfaces ; for-each-declaration
	define-record-types
	records
	signals
	bindings
	packages
	packages-internal
	locations
	environments
	ensures-loaded
	package-commands-internal)
  (files rt-module))

(define-structure with-locks with-locks-interface
  (open scheme
	locks)
  (files with-locks))

;;; EOF
;;; Local Variables:
;;; buffer-tag-table: "../../../TAGS"
;;; End::
