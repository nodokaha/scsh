;;; Code to parse information submitted from HTML forms. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; See http://www.w3.org/hypertext/WWW/MarkUp/html-spec/html-spec_toc.html

;;; About HTML forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The form's field data are turned into a single string, of the form
;;;     name=val&name=val
;;; where the <name> and <val> parts are URI encoded to hide their
;;; &, =, and + chars, among other things. After URI encoding, the
;;; space chars are converted to + chars, just for fun. It is important
;;; to encode the spaces this way, because the perfectly general %xx escape
;;; mechanism might be insufficiently confusing. This variant encoding is
;;; called "form-url encoding."
;;;
;;; If the form's method is POST,
;;;     Browser sends the form's field data in the entity block, e.g.,
;;;     "button=on&ans=yes". The request's Content-type: is application/
;;; 	x-www-form-urlencoded, and the request's Content-length: is the
;;; 	number of bytes in the form data.
;;;
;;; If the form's method is GET,
;;;     Browser sends the form's field data in the URL's <search> part.
;;;     (So the server will pass to the CGI script as $QUERY_STRING,
;;;     and perhaps also on in argv[]).
;;;
;;; In either case, the data is "form-url encoded" (as described above).

(define (parse-html-form-query q)
  (let ((qlen (string-length q)))
    (let recur ((i 0))
      (cond 
       ((>= i qlen) '())
       ((string-index q #\= i) =>
	(lambda (j)
	  (let ((k (or (string-index q #\& j) qlen)))
	    (cons (cons (unescape-uri+ q i j)
			(unescape-uri+ q (+ j 1) k))
		  (recur (+ k 1))))))
       (else '())))))			; BOGUS STRING -- Issue a warning.


;;; Map plus characters to spaces, then do URI decoding.
(define (unescape-uri+ s . maybe-start/end)
  (let-optionals maybe-start/end ((start 0)
				  (end (string-length s)))
    (unescape-uri (string-map (lambda (c) (if (char=? c #\+) #\space c))
			      (if (and (zero? start)
				       (= end (string-length s)))
				  s	; Gratuitous optimisation.
				  (substring s start end))))))
