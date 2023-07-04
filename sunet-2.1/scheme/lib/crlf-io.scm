;;; Read cr/lf and lf terminated lines. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers. <shivers@lcs.mit.edu>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; (read-crlf-line [fd/port retain-crlf?]) -> string or EOF object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a line terminated by either line-feed or EOF. If RETAIN-CRLF? is #f 
;;; (the default), a terminating cr/lf or lf sequence is trimmed from the
;;; returned string.
;;;
;;; This is simple and inefficient. It would be save one copy if we didn't
;;; use READ-LINE, but replicated its implementation instead.

(define (read-crlf-line . args)
  (let-optionals args ((fd/port (current-input-port))
		       (retain-crlf? #f))
    (let ((ln (read-line fd/port retain-crlf?)))
      (if (or retain-crlf? (eof-object? ln))
	  ln
	  (let ((slen (string-length ln)))	; Trim a trailing cr, if any.
	    (if (or (zero? slen)
		    (not (char=? (string-ref ln (- slen 1)) cr)))
		ln
		(substring ln 0 (- slen 1))))))))

(define cr (ascii->char 13))

(define (write-crlf port)
  (write-string "\r\n" port)
  (force-output port))

(define (read-crlf-line-timeout . args)
  (let-optionals args ((fd/port (current-input-port))
		       (retain-crlf? #f)
		       (timeout 8000)
		       (max-interval 500))
   (let loop ((waited 0) (interval 100))
     (cond ((> waited timeout)
	    'timeout)
	   ((char-ready? fd/port)
	    (read-crlf-line fd/port retain-crlf?))
	   (else (sleep interval)
		 (loop (+ waited interval) (min (* interval 2)
						max-interval)))))))
		 
		 
