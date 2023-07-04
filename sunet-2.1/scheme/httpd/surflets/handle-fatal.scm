;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error-Handler
;; 
;; Adopted from WITH-FATAL-ERROR-HANDLER, but handles everything that
;; is catchable. We must catch everything because we also want
;; exceptions (and warnings) to be catched (e.g. when the surflet is
;; loaded.)
(define (with-fatal-handler* handler thunk)
  (call-with-current-continuation
    (lambda (accept)
      ((call-with-current-continuation
         (lambda (k)
	   (with-handler (lambda (condition more)
			   (call-with-current-continuation
			    (lambda (decline)
			      (k (lambda () (handler condition decline)))))
			   (more))	; Keep looking for a handler.
	      (lambda () (call-with-values thunk accept)))))))))
		  
(define-syntax with-fatal-handler 
  (syntax-rules ()
    ((with-fatal-handler handler body ...)
     (with-fatal-handler* handler
       (lambda () body ...)))))