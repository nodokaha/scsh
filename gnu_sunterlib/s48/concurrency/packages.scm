(define-interface semaphores-interface
  (export make-semaphore
	  semaphore-post
	  semaphore-wait
	  with-semaphore-posted))

(define-interface with-lock-interface
  (export with-lock))(define-structure semaphores semaphores-interface
  (open scheme
	locks
	with-lock
	define-record-types)
  (files semaphore))

(define-structure with-lock with-lock-interface
  (open scheme 
	locks)
  (files with-lock))