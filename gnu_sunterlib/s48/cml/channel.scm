(define-record-type :channel
  (really-make-channel priority in out)
  channel?
  (priority channel-priority set-channel-priority!)
  ;; queue of trans-id * #f
  (in channel-in)
  ;; queue of trans-id * message
  (out channel-out))

(define-record-type :q-item
  (make-q-item trans-id message cleanup-thunk wrap-proc)
  q-item?
  (trans-id q-item-trans-id)
  (message q-item-message)
  (cleanup-thunk q-item-cleanup-thunk)
  (wrap-proc q-item-wrap-proc))

(define (make-channel)
  (really-make-channel 1 (make-queue) (make-queue)))

(define (channel=? channel-1 channel-2)
  (eq? channel-1 channel-2))

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

(define (send-rv channel message)
  (make-base
   (lambda ()
     (let ((in (channel-in channel)))
       (clean-queue-head! in)
       (if (queue-empty? in)
	   (make-blocked (lambda (trans-id cleanup-thunk wrap-proc)
			   (clean-and-enqueue! (channel-out channel)
					       (make-q-item trans-id
							    message
							    cleanup-thunk
							    wrap-proc))))
	   (let ((priority (channel-priority channel)))
	     (set-channel-priority! channel (+ 1 priority))
	     (make-enabled 
	      priority
	      (lambda ()
		(let ((q-item (dequeue! in)))
		  (set-channel-priority! channel 1)
		  ((q-item-cleanup-thunk q-item))
		  (cr-trans-id-wakeup (q-item-trans-id q-item)
				      (cons message
					    (q-item-wrap-proc q-item)))
		  (unspecific))))))))))

(define (send channel message)
  (sync (send-rv channel message)))

(define (receive-rv channel)
  (make-base
   (lambda ()
     (let ((out (channel-out channel)))
       (clean-queue-head! out)
       (if (queue-empty? out)
	   (make-blocked (lambda (trans-id cleanup-thunk wrap-proc)
			   (clean-and-enqueue! (channel-in channel)
					       (make-q-item trans-id
							    #f
							    cleanup-thunk
							    wrap-proc))))
	   (let ((priority (channel-priority channel)))
	     (set-channel-priority! channel (+ 1 priority))
	     (make-enabled
	      priority
	      (lambda ()
		(let ((q-item (dequeue! out)))
		  (set-channel-priority! channel 1)
		  ((q-item-cleanup-thunk q-item))
		  (cr-trans-id-wakeup (q-item-trans-id q-item)
				      (cons (unspecific)
					    (q-item-wrap-proc q-item)))
		  (q-item-message q-item))))))))))

(define (receive channel)
  (sync (receive-rv channel)))
