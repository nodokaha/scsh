;;; NCSA's WWW Common Gateway Interface -- script-side code -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; See http://hoohoo.ncsa.uiuc.edu/cgi/interface.html for a sort of "spec".

;;; This file provides routines to help you write programs in Scheme
;;; that can interface to HTTP servers using the CGI program interface
;;; to carry out HTTP transactions.

;;; ISINDEX queries:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (Likewise for ISINDEX URL queries from browsers.)
;;; Browser url-form encodes the query (see above), which then becomes the
;;; ?<search> part of the URI. (Hence the CGI script will split the individual
;;; fields into argv[].)


;;; CGI interface:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - The URL's <search> part is assigned to env var $QUERY_STRING, undecoded.
;;; - If it contains no raw "=" chars, it is split at "+" chars. The
;;;   substrings are URI decoded, and become the elts of argv[]. You aren't
;;;   supposed to rely on this unless you are replying to ISINDEX queries.
;;; - The CGI script is run with stdin hooked up to the socket. If it's going
;;;   to read the entity, it should read $CONTENT_LENGTH bytes worth.
;;; - A bunch of env vars are set with useful values.
;;; - Entity block is passed to script on stdin; 
;;;   script writes reply to stdout.
;;; - If the script begins with "nph-" its output is the entire reply.
;;;   Otherwise, when it replies to the server, it sends back a special
;;;   little header that tells the server how to construct the real header
;;;   for the reply.
;;; See the "spec" for further details. (URL above)

(define (cgi-form-query)
  (let ((request-method (getenv "REQUEST_METHOD")))
    (cond 

     ((string=? request-method "GET")
      (parse-html-form-query (getenv "QUERY_STRING")))

     ((string=? request-method "POST")
      (let ((nchars (string->number (getenv "CONTENT_LENGTH"))))
	(parse-html-form-query (read-string nchars))))

     (else (error "Method not handled."))))) ; Don't be calling me.
