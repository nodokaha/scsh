(define-interface mini-profiler-interface
  (export
   profile-init!
   display-profile
   (define-prof :syntax)
   (account-for :syntax)))

(define-structure mini-profiler mini-profiler-interface
  (open scheme
	table
	formats
	extended-ports
	time)
  (files profile))

(define-structure no-mini-profiler mini-profiler-interface
  (open scheme)
  (files no-profile))
