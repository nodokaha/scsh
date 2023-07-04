(define-interface trans-ids-interface
  (export enter-cr! leave-cr!
	  leave-cr-and-block!
	  trans-id?
	  make-trans-id
	  cr-trans-id-wait cr-trans-id-wakeup cr-maybe-trans-id-wakeup
	  trans-id-thread-uid trans-id-cancelled?))

(define-interface rendezvous-interface
  (export always-rv never-rv
	  guard with-nack choose wrap
	  sync
	  select))

(define-interface make-rendezvous-interface
  (export make-blocked make-enabled make-base))

(define-interface rendezvous-channels-interface
  (export make-channel
	  channel?
	  send-rv send
	  receive-rv receive))

(define-interface rendezvous-async-channels-interface
  (export make-async-channel
	  async-channel?
	  send-async
	  receive-async-rv
	  receive-async))

(define-interface rendezvous-placeholders-interface
  (export make-placeholder
	  placeholder?
	  placeholder-value
	  placeholder-set!
	  placeholder-value-rv))

(define-interface rendezvous-jars-interface
  (export make-jar
	  jar?
	  jar-take
	  jar-put!
	  jar-take-rv))

(define-structure trans-ids trans-ids-interface
  (open scheme
	srfi-9 big-util
	threads threads-internal interrupts
	locks placeholders)
  (files trans-id))

(define-structures ((rendezvous rendezvous-interface)
		    (make-rendezvous make-rendezvous-interface))
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids
	threads threads-internal
	big-util
	(subset util (unspecific)))
  (files rendezvous))

(define-structure rendezvous-channels rendezvous-channels-interface
  (open scheme
	srfi-9
	trans-ids rendezvous make-rendezvous
	queues
	big-util
	(subset util (unspecific)))
  (files channel))

(define-structure rendezvous-async-channels rendezvous-async-channels-interface
  (open scheme
	rendezvous
	rendezvous-channels
	threads
	queues
	srfi-9)
  (files async-channels))

(define-structure rendezvous-placeholders rendezvous-placeholders-interface
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids rendezvous make-rendezvous
	queues
	signals
	(subset util (unspecific)))
  (files placeholder))

(define-structure rendezvous-jars rendezvous-jars-interface
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids rendezvous make-rendezvous
	queues
	signals
	(subset util (unspecific)))
  (files jar))
