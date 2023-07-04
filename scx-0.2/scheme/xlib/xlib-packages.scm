(define-structure enum-sets-internal ;; exists but isn't accessible in scsh
  (export :enum-set-type)
  (open scheme primitives
	finite-types enum-sets
	external-calls)
  (begin
    (define-enumerated-type test :test test? tests test-name test-index
      (test1))
    (define-enum-set-type test-set :test-set test-set? make-test-set
      test test? tests test-index)
    (define test-value (test-set test1))
    (define :enum-set-type (record-ref test-value 0))
    (define-exported-binding "s48-enum-set-type" :enum-set-type)))

(define-structures ((xlib xlib-interface)
		    (xlib-internal xlib-internal-interface))
  (open rendezvous-channels
	scsh-level-0
	scheme
	list-lib
	srfi-13 ;; strings
	signals handle
	bitwise
	external-calls
	define-record-types
	finite-types
	enum-sets
	enum-sets-internal ;; for the enum-set-type
	placeholders
	threads
	ports locks
	channel-i/o
	interrupts
	ascii
	conditions)
  (files display
	 visual
	 colormap
	 cursor
	 pixmap
	 error
	 event event-types sync-event
	 font
	 gcontext
	 grab
	 graphics
	 key
	 property
	 text
	 window
	 wm
	 client
	 utility
	 atom
	 region))
