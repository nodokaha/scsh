;;; Error stuff for the http server. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Olin Shivers.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; An http error condition is a data structure with the following pieces:
;;;     (error-code request message . irritants)
;;; You recognise one with HTTP-ERROR?, and retrieve the pieces with
;;; CONDITION-STUFF.
;;;

;;; HTTP error condition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define a sub-type of the S48 error condition, the HTTP error condition.
;;; An HTTP error is one that corresponds to one of the HTTP error response
;;; codes, so you can reliably use an HTTP error condition to construct an
;;; error response message to send back to the HTTP client.

(define-condition-type 'http-error '(error))

(define http-error? (condition-predicate 'http-error))

(define (http-error status-code req . args)
  (apply signal 'http-error status-code req args))

;;; Syntax error condition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme 48 has a "syntax error" error condition, but it isn't an error
;;; condition! It's a warning condition. I don't understand this. 
;;; We define a *fatal* syntax error here for the parsers to use.

(define-condition-type 'fatal-syntax-error '(error))

(define fatal-syntax-error? (condition-predicate 'fatal-syntax-error))

(define (fatal-syntax-error msg . irritants)
  (apply signal 'fatal-syntax-error msg irritants))

