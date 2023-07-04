;;; extremly simple restartable conditions with SRFI-34/35

;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2004 by Eric Knauel.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; Example:

;;; (define-condition-type &harmless &condition
;;;   harmless-condition?)
;;; 
;;; (with-exception-handler
;;;  (lambda (c)
;;;    (cond 
;;;     ((harmless-condition? c)
;;;      (display "Oops!")
;;;      (newline)
;;;      (restart c))
;;;     (else 
;;;      (error (condition-message c)))))
;;;  (lambda ()
;;;    (display "Everything ok.")
;;;    (newline)
;;;    (raise-restartable (condition (&harmless)))
;;;    (display "Never mind.")
;;;    (newline)
;;;    (raise-restartable (condition (&message (message "Ouch!"))))))

(define-condition-type &restartable-condition &condition
  restartable-condition?
  (cont restartable-condition-cont))

(define-syntax raise-restartable
  (syntax-rules ()
    ((raise-restartable %obj)
     (call-with-current-continuation
      (lambda (restart-cont)
	(raise 
	 (make-compound-condition
	  (condition (&restartable-condition (cont restart-cont))) %obj)))))))

(define-syntax restart
  (syntax-rules ()
    ((restart %condition)
     (if (restartable-condition? %condition)
	 ((condition-ref (extract-condition %condition &restartable-condition) 'cont) 'ignored)
	 (raise (condition (&message (message "not a restartable condition"))))))))
