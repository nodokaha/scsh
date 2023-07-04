;; Copyright 2002, 2003 Andreas Bernauer

;; With callbacks you can create special links that are associated
;; with a function. If the user clicks on the special callback link,
;; the send-html/suspend won't return, but the function will be called
;; instead. 

;; NOTE: It is not sensible to create callbacks on top level, as they
;; contain continuations. You have to create a new callback every time
;; you want to use it (inside a function).
(define (make-callback function)
  (call-with-current-continuation
   (lambda (exit)
     (let* ((req (send/suspend (lambda (new-url)
				 (exit new-url)))))
       (function req)))))


(define (make-annotated-callback function)
  (let* ((annotated-address (make-annotated-address))
	 (dispatch 
	  (lambda (req)
	    (let ((bindings (get-bindings req)))
	      (cond
	       ((returned-via annotated-address bindings) =>
		(lambda (args)
		  (apply function (cons req args))))
	       (else
		(error "annotated-callback: 
unexpected return values from website"))))))
	 (callback (make-callback dispatch)))
    (lambda args
      (annotated-address callback args))))

(define callback-function
  (lambda (req proc . args) 
    (apply proc (cons req args))))
