(define-structure xpm xpm-interface
  (open scheme
	xlib
	signals primitives
	external-calls
	finite-types)
  (files xpm))

(define-structure xft xft-interface
  (open scheme
	xlib
	signals
	define-record-types
	finite-types
	srfi-1
	external-calls
	weak-table
	weak)
  (files xft))

(define-structure xrender xrender-interface
  (open scheme
	define-record-types
	external-calls)
  (files xrender))

(define-structure weak-table weak-table-interface
  (open scheme
	srfi-9
	general-tables
	weak)
  (files weak-table))
