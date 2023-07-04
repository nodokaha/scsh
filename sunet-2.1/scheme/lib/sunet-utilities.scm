; some useful utilities

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2002 by Andreas Bernauer.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (host-name-or-ip addr)
  (with-fatal-error-handler
   (lambda (condition more)
     (call-with-values
      (lambda () (socket-address->internet-address addr))
      (lambda (ip port)
	(format-internet-host-address ip))))
   (host-info:name (host-info addr))))

(define (on-interrupt interrupt thunk)
  (let lp ((event (most-recent-sigevent)))
    (let ((next (next-sigevent event interrupt)))
      (thunk)
      (lp next))))

(define (socket-address->string socket-address . with-port?)
  (let ((with-port? (:optional with-port? #t)))
    (receive (host-address service-port)
	     (socket-address->internet-address socket-address)
	     (if with-port?
		 (format #f "~A:~A"
			 (format-internet-host-address host-address)
			 (format-port service-port))
		 (format #f "~A"
			 (format-internet-host-address host-address))))))


;;; Assemble a filename from ROOT and the elts of PATH-LIST.
;;; If the assembled filename contains a .. subdirectory, return #f,
;;; otw return the filename.

(define dotdot-check
  (let ((dotdot-re (make-regexp "(^|/)\\.\\.($|/)")))	; Matches a .. subdir.
    (lambda (root path-list)
      (let ((fname (if (null? path-list) root	; Bogus hack.
		       (string-append (file-name-as-directory root)
				      (string-join path-list "/")))))
	(and (not (regexp-exec dotdot-re fname))	; Check for .. subdir.
	     fname)))))

;;; Timeout on network writes?

(define (copy-inport->outport in out . maybe-buffer-size)
  (let* ((buffer-size (:optional maybe-buffer-size 1024))
	 (buf (make-string buffer-size)))
    (let loop ()
      (cond ((read-string! buf in) => (lambda (nchars)
					(write-string buf out 0 nchars)
					(loop)))))
    (force-output out)))

(define (dump fd)
  (copy-inport->outport fd (current-output-port)))

(define (with-lock lock thunk)
  (dynamic-wind 
   (lambda ()
     (obtain-lock lock))
   thunk
   (lambda ()
     (release-lock lock))))


;; Get Header from (RFC822 like) header alist
(define (get-header headers tag)
  (cond ((assq tag headers) => cdr)
	(else #f)))
