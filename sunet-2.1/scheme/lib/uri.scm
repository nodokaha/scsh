;;; -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; URI syntax -- [scheme] : path [? search ] [# fragmentid]

;;; References:
;;; - http://www.w3.org/Addressing/rfc1630.txt
;;;   Original RFC
;;; - http://www.w3.org/hypertext/WWW/Addressing/URL/URI_Overview.html
;;;   General Web page of URI pointers.

(define uri-reserved (string->char-set ";/#?: ="))

(define uri-reserved-sans-= (char-set-delete uri-reserved #\=))

(define (parse-uri s)
  (let* ((slen (string-length s))
	 ;; Search forwards for colon (or intervening reserved char).
	 (rs1 (string-index s uri-reserved))	; 1st reserved char
	 (colon (and rs1 (char=? (string-ref s rs1) #\:) rs1))
	 (path-start (if colon (+ colon 1) 0))

	 ;; Search backwards for # (or intervening reserved char).
	 (rs-last (string-index-right s uri-reserved))
	 (sharp (and rs-last (char=? (string-ref s rs-last) #\#) rs-last))

	 ;; Search backwards for ? (or intervening reserved char).
	 ;; (NB: #\= may be after #\? and before #\#)
	 (rs-penult (string-index-right s                      
					uri-reserved-sans-=
					path-start
					(or sharp slen)))
	 (ques (and rs-penult (char=? (string-ref s rs-penult) #\?) rs-penult))

	 (path-end (or ques sharp slen)))
    (values (and colon (substring s 0 colon))
	    (split-uri s path-start path-end)
	    (and ques (substring s (+ ques 1) (or sharp slen)))
	    (and sharp (substring s (+ sharp 1) slen)))))

;;; Caution:
;;; Don't use this proc until *after* you've parsed the URL -- unescaping
;;; might introduce reserved chars (like slashes and colons) that could
;;; blow your parse.

(define (unescape-uri s . maybe-start/end)
  (let-optionals maybe-start/end ((start 0)
				  (end (string-length s)))
    (let* ((esc-seq? (lambda (i) (and (< (+ i 2) end)
				      (char=? (string-ref s i) #\%)
				      (hex-digit? (string-ref s (+ i 1)))
				      (hex-digit? (string-ref s (+ i 2))))))
	   (hits (let lp ((i start) (hits 0)) ; count # of esc seqs.
		   (if (< i end)
		       (if (esc-seq? i)
			   (lp (+ i 3) (+ hits 1))
			   (lp (+ i 1) hits))
		       hits))))
	 
      (if (and (zero? hits) (zero? start) (= end (string-length s)))
	  s
	  (let* ((nlen (- (- end start) (* hits 2))) ; the new length
						     ; of the
						     ; unescaped
						     ; string stores
						     ; the result
		 (ns (make-string nlen)))

	    (let lp ((i start) (j 0))	; sweep over the string
	      (if (< j nlen)
		  (lp (cond 
		       ((esc-seq? i)	; unescape
					; escape-sequence
			(string-set! ns j
				     (let ((d1 (string-ref s (+ i 1)))
					   (d2 (string-ref s (+ i 2))))
				       (ascii->char (+ (* 16 (hexchar->int d1))
						       (hexchar->int d2)))))
			(+ i 3))
		       (else (string-set! ns j (string-ref s i))  
			     (+ i 1)))
		      (+ j 1))))
	    ns)))))

(define hex-digit?
  (let ((hex-digits (string->char-set "0123456789abcdefABCDEF")))
    (lambda (c) (char-set-contains? hex-digits c))))

; make use of the fact that numbers and characters are in order in the ascii table
(define (hexchar->int c)      
  (- (char->ascii c) 
     (if (char-numeric? c)
	 (char->ascii #\0)         
	 (- (if (char-upper-case? c)
		(char->ascii #\A)
		(char->ascii #\a))
	    10))))

(define int->hexchar
  (let ((table '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		  #\A #\B #\C #\D #\E #\F)))
    (lambda (i) (vector-ref table i))))
  

;;; Caution:
;;; All reserved chars (e.g., slash, sharp, colon) get escaped: "=;/#?: "
;;; So don't apply this proc to chunks of text with syntactically meaningful
;;; reserved chars (e.g., paths with URI slashes or colons) -- they'll be 
;;; escaped, and lose their special meaning. E.g. it would be a mistake
;;; to apply ESCAPE-URI to "//lcs.mit.edu:8001/foo/bar.html" because the
;;; slashes and colons would be escaped.

(define uri-escaped-chars
  (char-set-complement 
   ;; RFC 2396 (URI Generic Syntax) specifies unreserved = alphanum | mark
   (char-set-union char-set:letter+digit
                   (string->char-set "-_.!~*'()"))))

;;; Takes a set of chars to escape. This is because we sometimes need to
;;; escape larger sets of chars for different parts of a URI.

(define (escape-uri s . maybe-escaped-chars)
  (let-optionals maybe-escaped-chars ((escaped-chars uri-escaped-chars))
    (let ((nlen (string-fold
		 (lambda (c i)
		   (+ i
		      (if (char-set-contains? escaped-chars c)
			  3
			  1)))
		 0
		 s)))    ; new length of escaped string
      (if (= nlen (string-length s))
	  s
	  (let ((ns (make-string nlen))) 
	    (string-fold
	     (lambda (c i)     ; replace each occurance of an
			       ; character to escape with %ff where ff
			       ; is the ascii-code in hexadecimal
			       ; notation
	       (+ i (cond 
		     ((char-set-contains? escaped-chars c)
		      (string-set! ns i #\%)
		      (let* ((d (char->ascii c))
			     (dhi (bitwise-and (arithmetic-shift d -4) #xF))
			     (dlo (bitwise-and d #xF)))
			(string-set! ns (+ i 1)
				     (int->hexchar dhi))
			(string-set! ns (+ i 2)
				     (int->hexchar dlo)))
		      3)
		     (else (string-set! ns i c)
			   1))))
	     0
	     s)
	    ns)))))

;;; Cribbed from scsh's fname.scm

(define (split-uri uri start end)		; Split at /'s (infix grammar).
  (let split ((i start))			; "" -> ("")
    (cond
     ((>= i end) '(""))
     ((string-index uri #\/ i) =>
      (lambda (slash)
	(cons (substring uri i slash)
	      (split (+ slash 1)))))
     (else (list (substring uri i end))))))


;;; The elements of PLIST must be escaped in case they contain slashes.
;;; This procedure doesn't escape them for you; you must do that yourself:
;;;     (uri-path->uri (map escape-uri pathlist))

(define (uri-path->uri plist)
  (string-join plist "/"))		; Insert slashes between elts of PLIST.

(define (simplify-uri-path p)    
  (if (null? p)
      #f				; P must be non-null
      (let lp ((path-list (cdr p))
	       (stack (list (car p))))
	(if (null? path-list)		; we're done
	    (reverse stack)
	    (cond
	     ((string=? (car path-list) "..") ; back up
					; neither the empty path nor root
	      (if (not (or (null? stack) (string=? (car stack) "")))  
		  (lp (cdr path-list) (cdr stack))
		  #f))
	     ((string=? (car path-list) ".") ; leave this
	      (lp (cdr path-list) stack))
	     ((string=? (car path-list) "") ; back to root
	      (lp (cdr path-list) '("")))
	     (else			; usual segment
	      (lp (cdr path-list) (cons (car path-list) stack))))))))
