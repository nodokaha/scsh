; Jars (multiple-assignment cells for use with threads)
; these are equivalent to ID-90 M-structures

(define-record-type :jar
  (really-make-jar priority queue value id)
  jar?
  (priority jar-priority set-jar-priority!)
  (queue jar-queue)
  (value jar-value set-jar-value!)
  (id jar-id))

(define the-empty-jar-value (list 'empty-jar))

(define (empty-jar-value? thing)
  (eq? thing the-empty-jar-value))

(define-record-discloser :jar
  (lambda (jar)
    (cons 'jar
	  (if (jar-id jar)
	      (list (jar-id jar))
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

(define (make-jar . id-option)
  (really-make-jar 0
		   (make-queue)
		   the-empty-jar-value
		   (if (null? id-option)
		       #f
		       (car id-option))))

(define (jar-take-rv jar)
  (make-base
   (lambda ()
     (cond
      ((empty-jar-value? (jar-value jar))
       (make-blocked
	(lambda (trans-id cleanup-thunk wrap-proc)
	  (clean-and-enqueue! (jar-queue jar)
			      (make-q-item trans-id
					   cleanup-thunk
					   wrap-proc)))))
      (else
       (let ((priority (jar-priority jar)))
	 (set-jar-priority! jar (+ 1 priority))
	 (make-enabled
	  priority
	  (lambda ()
	    (let ((value (jar-value jar)))
	      (set-jar-value! jar the-empty-jar-value)
	      value)))))))))

(define (jar-put! jar value)
  (enter-cr!)
  (cond
   ((empty-jar-value? (jar-value jar))
    (cond
     ((clean-and-dequeue! (jar-queue jar))
      => (lambda (q-item)
	   ((q-item-cleanup-thunk q-item))
	   (cr-trans-id-wakeup (q-item-trans-id q-item)
			       (cons value
				     (q-item-wrap-proc q-item)))))
     (else
      (set-jar-value! jar value)))
    (leave-cr!)
    (unspecific))
   (else
    (leave-cr!)
    (error "jar is already full" jar value))))

(define (jar-take jar)
  (sync (jar-take-rv jar)))
