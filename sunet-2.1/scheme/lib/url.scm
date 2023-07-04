;;; URL parsing and unparsing -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; I'm only implementing HTTP URL's right now.

;;; References:
;;; - http://www.w3.org/Addressing/rfc1738.txt
;;;   Original RFC
;;; - http://www.w3.org/hypertext/WWW/Addressing/URL/Overview.html
;;;   General Web page of URI pointers.


;;; Unresolved issues:
;;; - The server parser shouldn't substitute default values --
;;;   that should happen in a separate step.

;;; The steps in hacking a URL are:
;;; - Take the UID, parse it, and resolve it with the context UID, if any.
;;; - Consult the UID's <scheme>. Pick the appropriate URL parser and parse.


;;; Server strings: //<user>:<password>@<host>:<port>/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A SERVER record describes path-prefixes of the form
;;;     //<user>:<password>@<host>:<port>/
;;; These are frequently used as the initial prefix of URL's describing
;;; Internet resources.

(define-record-type server :server		; Each slot is a decoded string or #f.
  (make-server user password host port)
  server?
  (user server-user)
  (password server-password)
  (host server-host)
  (port server-port))

;;; Parse a URI path (a list representing a path, not a string!) into
;;; a server record. Default values are taken from the server
;;; record DEFAULT except for the host. Returns a server record if
;;; it wins. CADDR drops the server portion of the path. In fact,
;;; fatal-syntax-error is called, if the path doesn't start with '//'.

					; 
(define (parse-server path default)
  (if (and (pair? path)			; The thing better begin
	   (string=? (car path) "")	; with // (i.e., have two
	   (pair? (cdr path))		; initial "" elements).
	   (string=? (cadr path) ""))
	 
      (let* ((uhs (caddr path))		; Server string.
	     (uhs-len (string-length uhs))
	     (at (string-index uhs #\@)) ; Usr:passwd at-sign, if any.
	       
	     (colon1 (and at (string-index uhs #\:))) ; Usr:passwd colon,
	     (colon1 (and colon1 (< colon1 at) colon1))	; if any.

	     (colon2 (string-index uhs #\: (or at 0))))	; Host:port colon, if any.
	(make-server (if at
			 (unescape-uri uhs 0 (or colon1 at))
			 (server-user default))
		     (if colon1
			 (unescape-uri uhs (+ colon1 1) at)
			 (server-password default))
		     (unescape-uri uhs (if at (+ at 1) 0)
				   (or colon2 uhs-len))
		     (if colon2
			 (unescape-uri uhs (+ colon2 1) uhs-len)
			 (server-port default))))

      (fatal-syntax-error "URL must begin with //..." path)))

;;; Unparser

(define server-escaped-chars
  (char-set-union uri-escaped-chars	; @ and : are also special
		  (string->char-set "@:"))) ; in UH strings.

(define (server->string uh)
  (let* ((us (server-user uh))
	 (pw (server-password uh))
	 (ho (server-host uh))
	 (po (server-port uh))

	 ;; Encode before assembly in case pieces contain colons or at-signs.
	 (e (lambda (s) (escape-uri s server-escaped-chars)))

	 (user/passwd (if us
			  `(,(e us) . ,(if pw `(":" ,(e pw) "@") '("@")))
			  '()))
	 (host/port   (if ho
			  `(,(e ho) . ,(if po `(":" ,(e po)) '()))
			  '())))

    (apply string-append (append user/passwd host/port))))


;;; HTTP URL parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The PATH slot of this record is the URL's path split at slashes,
;;; e.g., "foo/bar//baz/" => ("foo" "bar" "" "baz" "")
;;; These elements are in raw, unescaped format. To convert back to
;;; a string, use (uri-path->uri (map escape-uri pathlist)).

(define-record-type http-url :http-url
  (make-http-url server path search fragment-identifier)
  http-url?
  (server http-url-server)		; Initial //anonymous@clark.lcs.mit.edu:80/
  (path http-url-path)			; Rest of path, split at slashes & decoded.
  (search http-url-search)
  (fragment-identifier http-url-fragment-identifier))

;;; The URI parser (parse-uri in uri.scm) maps a string to four parts:
;;; <scheme> : <path> ? <search> # <frag-id> <scheme>, <search>, and
;;; <frag-id> are strings; <path> is a non-empty string list -- the
;;; URI's path split at slashes. Optional parts of the URI, when
;;; missing, are specified as #f. If <scheme> is "http", then the
;;; other three parts can be passed to PARSE-HTTP-URL, which parses
;;; them into a HTTP-URL record. All strings come back from the URI
;;; parser encoded. SEARCH and FRAG-ID are left that way; this parser
;;; decodes the path elements.
;;;
;;; Returns a HTTP-URL record, if possible. Otherwise
;;; FATAL-SYNTAX-ERROR is called.

(define (parse-http-url path search frag-id)
  (let ((uh (parse-server path default-http-server)))
    (if (or (server-user uh) (server-password uh))
	(fatal-syntax-error
	 "HTTP URL's may not specify a user or password field" path))

    (make-http-url uh (map unescape-uri (cdddr path)) search frag-id)))

(define (parse-http-url-string string)
  (call-with-values
   (lambda () (parse-uri string))
   (lambda (scheme path search frag-id)
     (if (string=? scheme "http")
	 (parse-http-url path search frag-id)
	 (fatal-syntax-error "not an HTTP URL" path)))))

;;; Default http port is 80.
(define default-http-server (make-server #f #f #f "80"))


;;; Unparse.

(define (http-url->string url)
  (string-append "http://"
		 (server->string (http-url-server url))
		 "/"
		 (uri-path->uri (map escape-uri (http-url-path url)))
		 (cond ((http-url-search url) =>
			(lambda (s) (string-append "?" s)))
		       (else ""))
		 (cond ((http-url-fragment-identifier url) =>
			(lambda (fi) (string-append "#" fi)))
		       (else ""))))
