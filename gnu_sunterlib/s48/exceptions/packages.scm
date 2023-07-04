(define-interface srfi-34-interface
  (export
   raise
   with-exception-handler
   with-exception-handlers
   (guard :syntax)))

(define-interface srfi-35-interface
  (export
   make-condition-type
   condition-type?
   make-condition
   condition?
   condition-has-type?
   condition-ref
   make-compound-condition
   extract-condition
   (define-condition-type :syntax)
   (condition :syntax)
   &condition
   &message message-condition? condition-message
   &serious serious-condition?
   &error error?))

(define-interface srfi-34-restart-interface
  (export
   (raise-restartable :syntax)
   (restart :syntax)))

(define-structure srfi-34 srfi-34-interface
  (open scheme
	signals)
  (files srfi-34))

(define-structure srfi-35 srfi-35-interface
  (open scheme
	signals
	srfi-1
	srfi-9)
  (files srfi-35))

(define-structure srfi-34-restart srfi-34-restart-interface
  (open scheme
	srfi-34 srfi-35)
  (files restart))
