(define *cr-lock* (make-lock))

(define *cr-interrupt-mask* #f)

(define (enter-cr!)
  (let ((old-enabled (set-enabled-interrupts! no-interrupts)))
    (if (= old-enabled no-interrupts)
	(error "tried to enter critical region from critical region")
	(set! *cr-interrupt-mask* old-enabled))))

(define (leave-cr!)
  (set-enabled-interrupts! *cr-interrupt-mask*))

(define (leave-cr-and-block!)
  (leave-cr!)
  (block))

(define (in-cr?)
  (let* ((old-enabled (set-enabled-interrupts! no-interrupts))
	 (yes? (= old-enabled no-interrupts)))
    (set-enabled-interrupts! old-enabled)
    yes?))


;; This replaces trans-id REF in Reppy's code

(define-record-type :trans-id
  (really-make-trans-id maybe-thread-uid placeholder)
  trans-id?
  (maybe-thread-uid trans-id-maybe-thread-uid
		    set-trans-id-maybe-thread-uid!)
  (placeholder trans-id-placeholder set-trans-id-placeholder!))

(define (make-trans-id)
  (really-make-trans-id (thread-uid (current-thread))
			(make-placeholder)))

;; this stuff needs to move into WAKEUP
(define (cr-trans-id-wait trans-id)
  (if (not (in-cr?))
      (error "not in critical region"))
  (if (trans-id-cancelled? trans-id)
      (error "wait on cancelled trans-id"))
  (let ((placeholder (trans-id-placeholder trans-id)))
    (leave-cr!)
    (placeholder-value placeholder)))

(define (cr-trans-id-wakeup trans-id value)
  (if (not (in-cr?))
      (error "not in critical region"))
  (if (trans-id-cancelled? trans-id)
      (error "wakeup on cancelled trans-id"))
  (let ((placeholder (trans-id-placeholder trans-id)))
    (set-trans-id-maybe-thread-uid! trans-id #f)
    (set-trans-id-placeholder! trans-id 'no-placeholder)
    (placeholder-set! placeholder value)))

(define (cr-maybe-trans-id-wakeup trans-id value)
  (if (not (in-cr?))
      (error "not in critical region"))
  (if (not (trans-id-cancelled? trans-id))
      (cr-trans-id-wakeup trans-id value)))

(define (trans-id-cancelled? trans-id)
  (not (trans-id-maybe-thread-uid trans-id)))

(define (trans-id-thread-uid trans-id)
  (if (trans-id-cancelled? trans-id)
      (error "trans-id cancelled"))
  (trans-id-maybe-thread-uid trans-id))