;;; http server in the Scheme Shell	-*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1996 by Mike Sperber. <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; This code is very rudimentary at the moment and up for some expansion.
;;; Right now, it is primarily useful for running the server through a
;;; web accelerator

(define (access-denier . hosts)
  (lambda (info)
    (and (any (lambda (host)
		(host-matches? info host))
	      hosts)
	 'deny)))

(define (access-allower . hosts)
  (lambda (info)
    (and (any (lambda (host)
		 (host-matches? info host))
	      hosts)
	 'allow)))

(define (access-controller . controls)
  (lambda (info)
    (let loop ((controls controls))
      (and (pair? controls)
	   (or ((car controls) info)
	       (loop (cdr controls)))))))

(define (access-controlled-handler control ph)
  (lambda (path req)
    (if (eq?
	 (control (host-info (socket-remote-address (request-socket req))))
	 'deny)
	(http-error (status-code forbidden) req)
	(ph path req))))

(define (address->list address)
  (list (arithmetic-shift (bitwise-and address #xff000000) -24)
	(arithmetic-shift (bitwise-and address #xff0000) -16)
	(arithmetic-shift (bitwise-and address #xff00) -8)
	(bitwise-and address #xff)))
   
(define (host-matches? info host)
  (cond
   ((list? host)
    (let ((len (length host)))
      (any (lambda (address)
	     (equal? (take len (address->list address)) host))
	   (host-info:addresses info))))
   (else ; (string? host)
    (any (lambda (name)
	   (string-match host (string-map char-downcase name)))
	 (cons (host-info:name info)
	       (host-info:aliases info))))))

(define normalize-host
  (let ((split (infix-splitter (make-regexp "\\.")))
	(number (make-regexp "[0-9]+")))
    (lambda (host)
      (let ((components (split host)))
	(if (every (lambda (component)
		     (regexp-exec number component))
		   components)
	    (map string->number components)
	    host)))))

(define (take n l)
  (let loop ((n n) (l l) (r '()))
    (if (zero? n)
	(reverse r)
	(loop (- n 1) (cdr l) (cons (car l) r)))))