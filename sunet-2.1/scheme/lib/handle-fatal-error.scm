;;; Handle fatal errors in a sensible way. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; (with-fatal-error-handler* handler thunk)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call THUNK, and return whatever it returns. If THUNK signals a condition,
;;; and that condition is an error condition (or a subtype of error), then
;;; HANDLER gets a chance to handle it.
;;; The HANDLER proc is applied to two values: 
;;;     (HANDLER condition decline)
;;; HANDLER's continuation is WITH-FATAL-ERROR-HANDLER*'s; whatever HANDLER
;;; returns is returned from WITH-FATAL-ERROR-HANDLER. HANDLER declines to
;;; handle the error by throwing to DECLINE, a nullary continuation.
;;;
;;; Why is it called with-FATAL-error-handler*? Because returning to the
;;; guy that signalled the error is not an option.
;;;
;;; Why the nested outer pair of CALL/CC's? Well, what happens if the user's
;;; error handler *itself* raises an error? This could potentially give
;;; rise to an infinite loop, because WITH-HANDLER runs its handler in
;;; the original condition-signaller's context, so you'd search back for a
;;; handler, and find yourself again. For example, here is an infinite loop:
;;;
;;;   (with-handler (lambda (condition more)
;;;                   (display "Loop!")
;;;                   (error "ouch"))	; Get back, Loretta.
;;;     (lambda () (error "start me up")))
;;;
;;; I could require W-F-E-H* users to code carefully, but instead I make sure
;;; the user's fatal-error handler runs in w-f-e-h*'s handler context, so
;;; if it signals a condition, we'll start the search from there. That's the
;;; point of continuation K. When the original thunk completes successfully,
;;; we dodge the K hackery by using ACCEPT to make a normal return.

(define (with-fatal-error-handler* handler thunk)
  (call-with-current-continuation
    (lambda (accept)
      ((call-with-current-continuation
         (lambda (k)
	   (with-handler (lambda (condition more)
			   (if (error? condition)
			       (call-with-current-continuation
				 (lambda (decline)
				   (k (lambda () (handler condition decline))))))
			   (more))	; Keep looking for a handler.
	      (lambda () (call-with-values thunk accept)))))))))
		  
(define-syntax with-fatal-error-handler 
  (syntax-rules ()
    ((with-fatal-error-handler handler body ...)
     (with-fatal-error-handler* handler
       (lambda () body ...)))))

;This one ran HANDLER in the signaller's condition-handler context.
;It was therefore susceptible to infinite loops if you didn't code 
;your handler's carefully.
;
;(define (with-fatal-error-handler* handler thunk)
;  (call-with-current-continuation
;    (lambda (accept)
;      (with-handler (lambda (condition more)
;		      (if (error? condition)
;			  (call-with-current-continuation
;		            (lambda (decline)
;			      (accept (handler condition decline)))))
;		      (more))	; Keep looking for a handler.
;        thunk))))

;;; (%error-handler-cond kont eh-clauses cond-clauses)
;;; Transform error-handler clauses into COND clauses by wrapping continuation
;;; KONT around the body of each e-h clause, so that if it fires, the result
;;; is thrown to KONT, but if no clause fires, the cond returns to the default
;;; continuation.

;(define-syntax %error-handler-cond
;  (syntax-rules (=> else)
;
;   ((%error-handler-cond kont ((test => proc) clause ...) (ans ...))
;    (%error-handler-cond kont
;			 (clause ...)
;			 ((test => (lambda (v) (kont (proc v)))) ans ...)))
;
;   ((%error-handler-cond kont ((test body ...) clause ...) (ans ...))
;    (%error-handler-cond kont
;			 (clause ...)
;			 ((test (kont (begin body ...))) ans ...)))
;
;   ((%error-handler-cond kont ((else body ...)) (ans-clause ...))
;    (cond (else body ...) ans-clause ...))
;
;   ((%error-handler-cond kont () (ans-clause ...))
;    (cond ans-clause ...))))
