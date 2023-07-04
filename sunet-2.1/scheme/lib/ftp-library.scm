; Utility library for FTP clients and servers

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998-2002 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define *window-size* 4096)

(define (copy-port->port-binary input-port output-port)
  (let ((buffer (make-string *window-size*)))
    (let loop ()
      (cond
       ((read-string! buffer input-port)
	=> (lambda (length)
	     (write-string buffer output-port 0 length)
	     (loop))))))
  (force-output output-port))

(define (copy-port->port-ascii input-port output-port)
  (let loop ()
    (let ((line (read-line input-port 'concat)))
      (if (not (eof-object? line))
	  (let ((length (string-length line)))
	    (cond
	     ((zero? length)
	      'fick-dich-ins-knie)
	     ((char=? #\newline (string-ref line (- length 1)))
	      (write-string line output-port 0 (- length 1))
	      (write-crlf output-port))
	     (else
	      (write-string line output-port)))
	    (loop)))))
  (force-output output-port))

(define (copy-ascii-port->port input-port output-port)
  (let loop ()
    (let* ((line (read-crlf-line input-port
				 #f)))
      (if (not (eof-object? line))
	  (let ((length (string-length line)))
	    (write-string line output-port 0 length)
	    (newline output-port)
	    (loop)))))
  (force-output output-port))

(define *port-arg-regexp*
  (make-regexp "^([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)$"))

(define (parse-port-arg string)
  (cond
   ((regexp-exec *port-arg-regexp* string)
    => (lambda (match)
	 (let ((components
		(map (lambda (match-index)
		       (string->number
			(match:substring match match-index)))
		     '(1 2 3 4 5 6))))
	   (if (any (lambda (component)
		      (> component 255))
		    components)
	       (call-error "invalid PORT argument" parse-port-arg))
	   (apply
	    (lambda (a1 a2 a3 a4 p1 p2)
	      (let ((address (+ (arithmetic-shift a1 24)
				(arithmetic-shift a2 16)
				(arithmetic-shift a3 8)
				a4))
		    (port (+ (arithmetic-shift p1 8) p2)))
		 (values address port)))
	    components))))
   (else
    (call-error "invalid PORT argument" parse-port-arg))))

