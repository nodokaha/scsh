;;;; HTTP request

;;; This file is part of the Scheme Untergrund Networking package.
;;; Copyright (c) 1996 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;;; This code defines the http REQUEST data structure

(define-record-type request :request
  (make-request method uri url version headers socket)
  request?
  (method request-method)		; A string such as "GET", "PUT", etc.
  (uri request-uri)			; The escaped URI string as read from request line.
  (url request-url)			; An http URL record (see url.scm).
  (version request-version)		; A (major . minor) integer pair.
  (headers request-headers)		; An rfc822 header alist (see rfc822.scm).
  (socket request-socket))		; The socket connected to the client.

(define-record-discloser :request
  (lambda (req)
    (list 'request 
	  (request-method req)
	  (request-uri req)
	  (request-url req)
	  (request-version req)
	  (request-headers req)
	  (request-socket req))))
;;; A http protocol version is an integer  pair: (major . minor).

(define (version< v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
	   (< (cdr v1) (cdr v2)))))

(define (version<= v1 v2) (not (version< v2 v1)))

(define (v0.9-request? req)
  (version<= (request-version req) '(0 . 9)))


(define (version->string v)
  (string-append "HTTP/"
		 (number->string (car v))
		 "."
		 (number->string (cdr v))))

