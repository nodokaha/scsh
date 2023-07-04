(define (with-lock lock thunk)
  (obtain-lock lock)
  (let ((res (thunk)))
    (release-lock lock)
    res))

;; Thread-safe event queue *******************************************

(define-record-type sync-x-event :sync-x-event
  (really-make-sync-x-event event next)
  sync-x-event?
  (event sync-x-event-event)
  (next really-next-sync-x-event really-set-next-sync-x-event))

(define (make-sync-x-event event)
  (really-make-sync-x-event event (make-placeholder)))

(define (next-sync-x-event sync-x-event pred)
  (let ((next (placeholder-value (really-next-sync-x-event sync-x-event))))
    (if (pred (sync-x-event-event next))
	next
	(next-sync-x-event next pred))))

(define (set-next-sync-x-event! sync-x-event next-sync-x-event)
  (placeholder-set! 
   (really-next-sync-x-event sync-x-event) 
   next-sync-x-event))

(define *most-recent-sync-x-event* (make-sync-x-event 'no-event))
(define *most-recent-lock* (make-lock))

(define (init-sync-x-events dpy)
  (spawn
   (lambda ()
     (let lp ()
       (let ((next (wait-event dpy)))
	 (with-lock *most-recent-lock*
		    (lambda ()
		      (set-next-sync-x-event! *most-recent-sync-x-event*
					      (make-sync-x-event next))
		      (set! *most-recent-sync-x-event*
			    (placeholder-value (really-next-sync-x-event
						*most-recent-sync-x-event*)))))
	 (lp))))
   'init-sync-x-events))

(define (most-recent-sync-x-event)
  *most-recent-sync-x-event*)

;; High-Level Event-Dispatcher ***************************************

;; contains (display window event-mask) triples
(define *event-requests* '())
(define *event-requests-lock* (make-lock))

(define (make-request display window event-mask)
  (list display window event-mask))

(define (add-request! req)
  (with-lock *event-requests-lock*
	     (lambda ()
	       (set! *event-requests*
		     (cons req *event-requests*))
	       (select-requests))))

(define (remove-request! req)
  (with-lock *event-requests-lock*
	     (lambda ()
	       (set! *event-requests*
		     (filter (lambda (r) (not (eq? r req))) *event-requests*))
	       (select-requests))))

(define request:display car)
(define request:window cadr)
(define request:event-mask caddr)

(define (event-masks-union masks)
  (fold-right (lambda (m res)
		(enum-set-union m res))
	      (event-mask)
	      masks))

(define (select-requests)
  (let loop ((rest *event-requests*))
    (if (not (null? rest))
	(let ((r (car rest)))
	  (call-with-values
	   (lambda ()
	     (partition (lambda (r2)
			  ;; find all with the same display and window
			  (and (eq? (request:display r2) (request:display r))
			       (eq? (request:window r2) (request:window r))))
			(cdr rest)))
	   (lambda (same rest)
	     (let ((mask (event-masks-union (map request:event-mask
						 (cons r same)))))
	       (if (window-exists? (request:display r) (request:window r))
		   (display-select-input (request:display r) (request:window r)
					 mask)))
	     (loop rest)))))))

(define (call-with-event-channel display window event-mask fun)
  (let ((r (make-request display window event-mask))
	(x-event-channel (make-channel))
        (filter-control-channel (make-channel))
        (dead?-box (cons #f #f)))
    (spawn-event-filter x-event-channel filter-control-channel 
                        display window event-mask fun dead?-box)
    ;; we send the first sync-event to the thread to really have the
    ;; most recent one, without keeping it in an environment.
    (send filter-control-channel (most-recent-sync-x-event))
    (let ((first? #t))
      (dynamic-wind
       (lambda () 
         (add-request! r)
         (if first?
             (set! first? #f)
             (warn "throwing back into call-with-event-channel")))
       (lambda () (call-with-values 
                   (lambda () (fun x-event-channel))
                   (lambda args
                     (apply values args))))
       (lambda ()
         (set-car! dead?-box #t)
         (remove-request! r))))))

(define (true x) #t)

(define (spawn-event-filter out-channel control-channel display window event-mask fun dead?-box)
  (let ((pred (lambda (e)
		(and (eq? (any-event-display e) display)
		     (matches-event-mask? window event-mask e)))))
    (spawn (lambda ()
	     ;; the first sync-event is send to us through the channel
	     (let loop ((se (receive control-channel)))
               (if (not (car dead?-box))
                   (let ((nse (next-sync-x-event se true)))
                     (if (not (car dead?-box))
                         (if (pred (sync-x-event-event nse))
                             (begin (send out-channel (sync-x-event-event nse))
                                    (loop nse))
                             (loop nse)))))))
           (cons 'spawn-event-filter fun))))

(define (matches-event-mask? window event-mask event)
  (let ((type (any-event-type event)))
    (cond
     ;; keymap-event has no window element
     ((eq? type (event-type keymap-notify))
      (enum-set-member? event-mask (event-mask-item keymap-state)))
     ;; other events must have at least the correct window
     ((not (eq? window (any-event-window event)))
      #f)
     ;; these event are send always because they do not depend on a mask
     ((or (eq? type (event-type client-message))
	  (eq? type (event-type mapping-notify))
	  (eq? type (event-type selection-clear))
	  (eq? type (event-type selection-notify))
	  (eq? type (event-type selection-request)))
      #t)
     ;; these do not depend an an event-mask too, but on a flag in GC,
     ;; so we sent it too
     ((or (eq? type (event-type graphics-expose))
	  (eq? type (event-type no-expose)))
      #t)

     ;; OwnerGrabButtonMask only generates extra events between a
     ;; ButtonPress and ButtonRelease event and does not be respected
     ;; here

     ;; PointerMotionHintMask only has an effect if one of the
     ;; ButtonMotion Masks or PointerMotionMask is selected, so we
     ;; don't have to take a look at it here.

     ;; for the rest one of the event-mask items must match the type
     ((any (lambda (mask-item)
	     (matches-event-mask-2? type window event mask-item))
	   (enum-set->list event-mask))
      #t)
     (else #f))))

(define (matches-event-mask-2? type window event mask-item)
  (cond
   ((or (eq? mask-item (event-mask-item button-motion))
	(eq? mask-item (event-mask-item button-1-motion))
	(eq? mask-item (event-mask-item button-2-motion))
	(eq? mask-item (event-mask-item button-3-motion))
	(eq? mask-item (event-mask-item button-4-motion))
	(eq? mask-item (event-mask-item button-5-motion)))
    (eq? type (event-type motion-notify)))
   ((eq? mask-item (event-mask-item button-press))
    (eq? type (event-type button-press)))
   ((eq? mask-item (event-mask-item button-release))
    (eq? type (event-type button-release)))
   ((eq? mask-item (event-mask-item colormap-change))
    (eq? type (event-type colormap-notify)))
   ((eq? mask-item (event-mask-item enter-window))
    (eq? type (event-type enter-notify)))
   ((eq? mask-item (event-mask-item leave-window))
    (eq? type (event-type leave-notify)))
   ((eq? mask-item (event-mask-item exposure))
    (eq? type (event-type expose)))
   ((eq? mask-item (event-mask-item focus-change))
    (or (eq? type (event-type focus-in))
	(eq? type (event-type focus-out))))
   ((eq? mask-item (event-mask-item keymap-state))
    (eq? type (event-type keymap-notify)))
   ((eq? mask-item (event-mask-item key-press))
    (eq? type (event-type key-press)))
   ((eq? mask-item (event-mask-item key-release))
    (eq? type (event-type key-release)))
   ((eq? mask-item (event-mask-item pointer-motion))
    (eq? type (event-type motion-notify)))
   ((eq? mask-item (event-mask-item property-change))
    (eq? type (event-type property-notify)))
   ((eq? mask-item (event-mask-item resize-redirect))
    (eq? type (event-type resize-request)))
   ((eq? mask-item (event-mask-item structure-notify))
    (or (and (eq? type (event-type circulate-notify))
	     (eq? window (circulate-event-event event))
	     (eq? window (circulate-event-window event)))
	(and (eq? type (event-type configure-notify))
	     (eq? window (configure-event-event event))
	     (eq? window (configure-event-window event)))
	(and (eq? type (event-type destroy-notify))
	     (eq? window (destroy-window-event-event event))
	     (eq? window (destroy-window-event-window event)))
	(and (eq? type (event-type gravity-notify))
	     (eq? window (gravity-event-event event))
	     (eq? window (gravity-event-window event)))
	(and (eq? type (event-type map-notify))
	     (eq? window (map-event-event event))
	     (eq? window (map-event-window event)))
	(and (eq? type (event-type reparent-notify))
	     (eq? window (reparent-event-event event))
	     (eq? window (reparent-event-window event)))
	(and (eq? type (event-type unmap-notify))
	     (eq? window (unmap-event-event event))
	     (eq? window (unmap-event-window event)))))
   ((eq? mask-item (event-mask-item substructure-notify))
    (or (and (eq? type (event-type circulate-notify))
	     (eq? window (circulate-event-event event))
	     (not (eq? window (circulate-event-window event))))
	(and (eq? type (event-type configure-notify))
	     (eq? window (configure-event-event event))
	     (not (eq? window (configure-event-window event))))
	(and (eq? type (event-type create-notify))
	     (eq? window (create-window-event-parent event))
	     (not (eq? window (create-window-event-window event))))
	(and (eq? type (event-type destroy-notify))
	     (eq? window (destroy-window-event-event event))
	     (not (eq? window (destroy-window-event-window event))))
	(and (eq? type (event-type gravity-notify))
	     (eq? window (gravity-event-event event))
	     (not (eq? window (gravity-event-window event))))
	(and (eq? type (event-type map-notify))
	     (eq? window (map-event-event event))
	     (not (eq? window (map-event-window event))))
	(and (eq? type (event-type reparent-notify))
	     (eq? window (reparent-event-event event))
	     (not (eq? window (reparent-event-window event))))
	(and (eq? type (event-type unmap-notify))
	     (eq? window (unmap-event-event event))
	     (not (eq? window (unmap-event-window event))))))
   ((eq? mask-item (event-mask-item substructure-redirect))
    (or (eq? type (event-type circulate-request))
	(eq? type (event-type configure-request))
	(eq? type (event-type map-request))))
   ((eq? mask-item (event-mask-item visibility-change))
    (eq? type (event-type visibility-notify)))
   (else #f)))
