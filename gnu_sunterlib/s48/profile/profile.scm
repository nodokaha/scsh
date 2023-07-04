;;; This file is part of the Scheme Untergrund Library.
;;;
;;; Copyright (c) 2000 by Matthias Neubauer
;;; Copyright (c) 2004 by Eric Knauel
;;;
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.
;;;
;;;
;;; ,open table formats extended-ports time

(define *profile-table*
  (make-symbol-table))

(define (profile-init!)
  (set! *profile-table* (make-symbol-table)))

(define (account-time! name run-time)
  (cond
   ((table-ref *profile-table* name)
    => (lambda (count.time)
	 (table-set! *profile-table* name 
		     (cons (+ 1 (car count.time))
			   (+ run-time (cdr count.time))))))
   (else
    (table-set! *profile-table* name (cons 0 run-time)))))

(define (hundredths n) 
  (let ((n (round (quotient n 10))))
    (string-append
     (number->string (quotient n 100))
     "."
     (let ((r (remainder n 100)))
       (if (< r 10)
	   "0"
	   (number->string r))))))

(define (table->string table)
  (let ((port (make-string-output-port)))
    (table-walk
     (lambda (key count.time)
       (format port "~A: ~A ~A~%"
	       key (car count.time) 
	       (hundredths (cdr count.time))))
     table)
    (string-output-port-output port)))

(define (display-profile . port)
  (let ((port (if (null? port) (current-output-port) port)))
    (format port
	    (string-append
	     "Profile summary~%"
	     "---------------~%"
	     "~A~%~%")
	    (table->string *profile-table*))))

(define-syntax define-prof
  (syntax-rules
   ()
   ((_ (?name . ?arg) ?body ...)
    (define (?name . ?arg)
      (define-prof "body" ?name ?body ...)))
   ((_ (?name ?args ...) ?body ...)
    (define (?name ?args ...)
      (define-prof "body" ?name ?body ...)))
   ((_ "body" ?name ?body ...)
    (let ((start-run-time (run-time))) 
      (call-with-values 
       (lambda ()
	 ?body ...)
       (lambda results
	 (let* ((stop-run-time (run-time))
		(run-time (- stop-run-time start-run-time)))
	   (account-time! (quote ?name) run-time)
	   (apply values results))))))))

(define-syntax account-for
  (syntax-rules
      ()
    ((account-for ?account ?body ...)
     (let ((start-run-time (run-time)))
       (call-with-values 
	(lambda ()
	  ?body ...)
	(lambda results
	  (let* ((stop-run-time (run-time))
		 (run-time (- stop-run-time start-run-time)))
	    (account-time! (quote ?account) run-time)
	    (apply values results))))))))
