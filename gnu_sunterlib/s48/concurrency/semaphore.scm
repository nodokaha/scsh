;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define-record-type semaphore :semaphore
  (really-make-semaphore sync-lock waiting free)
  (sync-lock semaphore-sync-lock)
  (waiting semaphore-waiting set-semaphore-waiting!)
  (free semaphore-free set-semaphore-free!))

(define (make-semaphore init-free)
  (really-make-semaphore (make-lock) '() init-free))

(define (semaphore-post sem)
  (with-lock (semaphore-sync-lock sem)
    (lambda ()
      (let ((waiting (semaphore-waiting sem)))
	(if (null? waiting)
	    (set-semaphore-free! sem (+ (semaphore-free sem) 1))
	    (let ((runnable (car waiting)))
	      (set-semaphore-waiting! sem (cdr waiting))
	      (release-lock runnable)))))))

(define (semaphore-wait sem)
  (obtain-lock (semaphore-sync-lock sem))
  (if (> (semaphore-free sem) 0)
      (begin
	(set-semaphore-free! sem (- (semaphore-free sem) 1))
	(release-lock (semaphore-sync-lock sem)))
      (let ((my-lock (make-lock)))
	(set-semaphore-waiting! sem (cons my-lock (semaphore-waiting sem)))
	(obtain-lock my-lock)
	(release-lock (semaphore-sync-lock sem))
	(obtain-lock my-lock))))
(define (with-semaphore-posted sem thunk)
  (dynamic-wind
   (lambda () (semaphore-wait sem))
   thunk
   (lambda () (semaphore-post sem))))