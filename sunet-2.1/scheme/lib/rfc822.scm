;;; RFC 822 field-parsing code

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers <shivers@lcs.mit.edu>
;;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; RFC 822 is the "Standard for the format of ARPA Internet text messages"
;;; -- the document that essentially tells how the fields in email headers
;;; (e.g., the Subject: and To: fields) are formatted. This code is for 
;;; parsing these headers.

;;; Here is a pointer to the document:
;;;    http://www.ietf.org/rfc/rfc0822.txt

;;; RFC 822 parsing is useful in other contexts as well -- the HTTP protocol
;;; uses it, and it tends to pop up here and there.
;;;
;;; RFC 822 header syntax has two levels: the general syntax for headers,
;;; and the syntax for specific headers. For example, once you have figured
;;; out which chunk of text is the To: line, there are more rules telling
;;; how to split the To: line up into a list of addresses. Another example:
;;; lines with dates, e.g., the Date: header, have a specific syntax for
;;; the time and date.
;;;
;;; This code currently *only* provides routines for parsing the gross
;;; structure -- splitting the message header into its distinct fields.
;;; It would be nice to provide the finer-detail parsers, too. You do it.
;;;     -Olin

;;; A note on line-terminators:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line-terminating sequences are always a drag, because there's no agreement
;;; on them -- the Net protocols and DOS use cr/lf; Unix uses lf; the Mac
;;; uses cr. One one hand, you'd like to use the code for all of the above,
;;; on the other, you'd also like to use the code for strict applications
;;; that need definitely not to recognise bare cr's or lf's as terminators.
;;;
;;; RFC 822 requires a cr/lf (carriage-return/line-feed) pair to terminate
;;; lines of text. On the other hand, careful perusal of the text shows up
;;; some ambiguities (there are maybe three or four of these, and I'm too
;;; lazy to write them all down). Furthermore, it is an unfortunate fact
;;; that many Unix apps separate lines of RFC 822 text with simple linefeeds
;;; (e.g., messages kept in /usr/spool/mail). As a result, this code takes a 
;;; broad-minded view of line-terminators: lines can be terminated by either
;;; cr/lf or just lf, and either terminating sequence is trimmed.
;;;
;;; If you need stricter parsing, you can pass a read-line procedure
;;; as an extra parameter. This means that you can pass in a procedure
;;; that recognizes only cr/lf's, or only cr's (for a Mac app,
;;; perhaps), and you can determine whether or not the terminators get
;;; trimmed. However, your read-line procedure must indicate the
;;; header-terminating empty line by returning *either* the empty
;;; string or the two-char string cr/lf (or the EOF object).

(define htab (ascii->char 9))

;;; Convert to a symbol using the Scheme implementation's preferred case,
;;; so we can compare these things against quoted constants.
(define string->symbol-pref
  (if (char=? #\a (string-ref (symbol->string 'a) 0))	; Is it #\a or #\A?
      (lambda (s) (string->symbol (string-map char-downcase s)))
      (lambda (s) (string->symbol (string-map char-upcase s)))))

(define (read-rfc822-field . args)
  (receive (field body)
      (apply read-rfc822-field-with-line-breaks args)
    (values field
	    (string-concatenate body))))

(define (read-rfc822-field-with-line-breaks . args)
  (let-optionals args ((port (current-input-port))
		       (read-line read-crlf-line))
    (let ((line1 (read-line port)))
      (if (or (eof-object? line1)
	      (zero? (string-length line1))
	      (string=? line1 "\r\n"))	; In case read-line doesn't trim.
	  (values #f #f)
	  (cond
	   ((string-index line1 #\:) =>
	    (lambda (colon)
	      (let ((name (string->symbol-pref (substring line1 0 colon))))
		;; Read in continuation lines.
		(let lp ((lines (list (substring line1
						 (+ colon 1)
						 (string-length line1)))))
		  (let ((c (peek-char port)))
		    ;;  RFC822: continuous lines has to start with a space or a htab 
		    (if (or (eqv? c #\space) (eqv? c htab))
			(lp (cons (read-line port) lines))
			(values name (reverse lines))))))))
	   (else (error "Illegal RFC 822 field syntax." line1))))))) ; No :

(define (make-read-rfc822-headers read-field)
  (lambda args
    (let-optionals args ((port (current-input-port))
			 (read-line read-crlf-line))
      (let lp ((alist '()))
	(receive (field val)
	    (read-field port read-line)
	  (if field
	      (lp (cons (cons field val) alist))
	      (reverse alist)))))))

(define read-rfc822-headers
  (make-read-rfc822-headers read-rfc822-field))
(define read-rfc822-headers-with-line-breaks
  (make-read-rfc822-headers read-rfc822-field-with-line-breaks))

(define (rfc822-time->string time)
  (format-date " ~a, ~d ~b ~Y ~H:~M:~S GMT" (date time 0)))
