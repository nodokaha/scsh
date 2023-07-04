;;; http server in the Scheme Shell	-*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; Copyright (c) 1996-2002 by Mike Sperber.
;;; Copyright (c) 2002 by Andreas Bernauer.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; Path handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path handlers are the guys that actually perform the requested operation
;;; on the URL. The handler interface is 
;;;     (handler path-list request)
;;; The path-list is a URL path list that is a suffix of REQUEST's url's
;;; path-list. Path handlers can decide how to handle an operation by
;;; recursively keying off of the elements in path-list. 
;;; 
;;; The object-oriented view:
;;; One way to look at this is to think of the request's METHOD as a
;;; generic operation on the URL. Recursive request handlers do method 
;;; lookup to determine how to implement a given operation on a particular
;;; path.
;;;
;;; The REQUEST is a request record, as defined in httpd-core.scm, containing
;;; the details of the client request.

;; general request handler combinator:
;; predicate: path x request --> boolean
;; if #t, handler is called
;; if #f, default-handler is called
(define (make-predicate-handler predicate handler default-handler)
  (lambda (path req)
    (if (predicate path req)
	(handler path req)
	(default-handler path req))))

;; same as MAKE-PREDICATE-HANDLER except that the predicate is only
;; called with the path:
;; predicate: path --> boolean
(define (make-path-predicate-handler predicate handler default-handler)
  (make-predicate-handler
   (lambda (path req) (predicate path)) handler default-handler))

;; selects handler according to host-field of http-request
(define (make-host-name-handler hostname handler default-handler)
  (make-predicate-handler 
   (lambda (path req)
     ;; we expect only one host-header-field
     (let ((body (string-trim (get-header (request-headers req) 'host))))
       (or (string-ci=? hostname body)
	   (string-prefix-ci? (string-append hostname ":") body))))
   handler default-handler))

(define (get-header headers tag)
  (cond
   ((assq tag headers) => cdr)
   (else
    (http-error (status-code bad-request) #f
		(string-append "Request did not contain "
			       (symbol->string tag)
			       " header")))))

;; selects handler according to path-prefix
;; if path-prefix matches, handler is called without the path-prefix
(define (make-path-prefix-handler path-prefix handler default-handler)
  (lambda (path req)
    (if (and (pair? path) (string=? path-prefix (car path)))
	(handler (cdr path) req)
	(default-handler path req))))

;;; (alist-path-dispatcher handler-alist default-handler) -> handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function creates a table-driven request handler that dispatches off
;;; of the car of the request path. The handler uses the car to index into
;;; a request handler alist. If it finds a hit, it recurses using the table's
;;; request handler. If no hits, it handles the path with a default handler.
;;; An alist handler is passed the tail of the original path; the
;;; default handler gets the entire original path.
;;;
;;; This procedure is how you say: "If the first element of the URL's
;;; path is 'foo', do X; if it's 'bar', do Y; otherwise, do Z." 

(define (alist-path-dispatcher handler-alist default-handler)
  (fold-right 
   (lambda (handler-pair default-handler)
     (make-path-prefix-handler
      (car handler-pair)
      (cdr handler-pair)
      default-handler))
   default-handler
   handler-alist))

;;; The null request handler -- handles nothing, sends back an error response.
;;; Can be useful as the default in table-driven request handlers.

(define (null-request-handler path req)
  (make-error-response (status-code not-found) req))
