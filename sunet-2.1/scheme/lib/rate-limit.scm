;;; Rate limiting -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2002 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define-record-type rate-limiter :rate-limiter
  (really-make-rate-limiter simultaneous-requests
			    access-lock
			    block-lock
			    current-requests)
  rate-limiter?
  (simultaneous-requests rate-limiter-simultaneous-requests)
  (access-lock rate-limiter-access-lock)
  (block-lock rate-limiter-block-lock)
  (current-requests rate-limiter-current-requests-unsafe
		    set-rate-limiter-current-requests!))

(define (make-rate-limiter simultaneous-requests)
  (really-make-rate-limiter simultaneous-requests
			    (make-lock)
			    (make-lock)
			    0))

(define (rate-limit-block rate-limiter)
  (obtain-lock (rate-limiter-block-lock rate-limiter)))

(define (rate-limit-open rate-limiter)
  (obtain-lock (rate-limiter-access-lock rate-limiter))
  (let ((current-requests
	 (+ 1 (rate-limiter-current-requests-unsafe rate-limiter))))
    (set-rate-limiter-current-requests! rate-limiter
					current-requests)
    (if (>= current-requests
	    (rate-limiter-simultaneous-requests rate-limiter))
	(maybe-obtain-lock (rate-limiter-block-lock rate-limiter))
	(release-lock (rate-limiter-block-lock rate-limiter))))
  (release-lock (rate-limiter-access-lock rate-limiter)))

(define (rate-limit-close rate-limiter)
  (obtain-lock (rate-limiter-access-lock rate-limiter))
  (let ((current-requests
	 (- (rate-limiter-current-requests-unsafe rate-limiter) 1)))
    (if (negative? current-requests)
	(error "rate-limiter: too many close operations"
	       rate-limiter))
    (set-rate-limiter-current-requests! rate-limiter
					current-requests)
    (if (= current-requests
	   (- (rate-limiter-simultaneous-requests rate-limiter)
	      1))
	;; we just came back into range
	(release-lock (rate-limiter-block-lock rate-limiter))))
  (release-lock (rate-limiter-access-lock rate-limiter)))

(define (rate-limiter-current-requests rate-limiter)
  (obtain-lock (rate-limiter-access-lock rate-limiter))
  (let ((current-requests
	 (rate-limiter-current-requests-unsafe rate-limiter)))
    (release-lock (rate-limiter-access-lock rate-limiter))
    current-requests))
