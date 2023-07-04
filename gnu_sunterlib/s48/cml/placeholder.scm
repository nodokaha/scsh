; Placeholders (single-assignment cells for use with threads)

(define-record-type :placeholder
  (really-make-placeholder priority queue value id)
  placeholder?
  (priority placeholder-priority set-placeholder-priority!)
  (queue placeholder-queue set-placeholder-queue!)
  (value placeholder-value-internal set-placeholder-value!)
  (id placeholder-id))

(define-record-discloser :placeholder
  (lambda (placeholder)
    (cons 'placeholder
	  (if (placeholder-id placeholder)
	      (list (placeholder-id placeholder))
	      '()))))

(define-record-type :q-item
  (make-q-item trans-id cleanup-thunk wrap-proc)
  q-item?
  (trans-id q-item-trans-id)
  (cleanup-thunk q-item-cleanup-thunk)
  (wrap-proc q-item-wrap-proc))

(define (clean-and-enqueue! queue value)
  (clean-queue-head! queue)
  (enqueue! queue value))

(define (clean-and-dequeue! queue)
  (let loop ()
    (if (queue-empty? queue)
	#f
	(let ((front (dequeue! queue)))
	  (if (trans-id-cancelled? (q-item-trans-id front))
	      (loop)
	      front)))))

(define (clean-queue-head! queue)
  (let loop ()
    (if (not (queue-empty? queue))
	(let ((front (queue-front queue)))
	  (if (trans-id-cancelled? (q-item-trans-id front))
	      (begin
		(dequeue! queue)
		(loop)))))))

(define (make-placeholder . id-option)
  (really-make-placeholder 0
			   (make-queue)
			   (unspecific)
			   (if (null? id-option)
			       #f
			       (car id-option))))

(define (placeholder-value-rv placeholder)
  (make-base
   (lambda ()
     (cond
      ((placeholder-queue placeholder)
       => (lambda (queue)
	    (make-blocked
	     (lambda (trans-id cleanup-thunk wrap-proc)
	       (clean-and-enqueue! queue
				   (make-q-item trans-id
						cleanup-thunk
						wrap-proc))))))
      (else
       (let ((priority (placeholder-priority placeholder)))
	 (set-placeholder-priority! placeholder (+ 1 priority))
	 (make-enabled
	  priority
	  (lambda ()
	    (placeholder-value-internal placeholder)))))))))

(define (placeholder-set! placeholder value)
  (enter-cr!)
  (cond
   ((placeholder-queue placeholder)
    => (lambda (queue)
	 (set-placeholder-value! placeholder value)
	 (set-placeholder-queue! placeholder #f)
	 (let loop ()
	   (cond
	    ((clean-and-dequeue! queue)
	     => (lambda (q-item)
		  ((q-item-cleanup-thunk q-item))
		  (cr-trans-id-wakeup (q-item-trans-id q-item)
				      (cons value
					    (q-item-wrap-proc q-item)))
		  (loop)))))
	 (leave-cr!)
	 (unspecific)))
   (else
    (leave-cr!)
    (error "placeholder is already assigned" placeholder value))))

(define (placeholder-value placeholder)
  (sync (placeholder-value-rv placeholder)))


     