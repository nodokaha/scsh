;; utilities for surflets
;; Copyright 2002, 2003 Andreas Bernauer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from parse-html-forms (cgi-script)
;;; Return the form data as an alist of decoded strings.
;;; So a query string like "button=on&reply=Oh,%20yes" becomes alist
;;;     (("button" . "on") ("reply" . "Oh, yes"))
;;; This works only for GET and POST methods.

(define (form-query-list q)
  (if q 
      (parse-html-form-query q) 
      '()))

;; from uri.scm
(define (rev-append a b)		; (append (reverse a) b)
  (let rev-app ((a a) (b b))		; Should be defined in a list-proc
    (if (pair? a)			; package, not here.
	(rev-app (cdr a) (cons (car a) b))
	b)))

;; Every call will surely return another number.
(define generate-unique-number
  (let ((id 0))
    (lambda ()
      (set! id (+ 1 id))
      id)))

;; FIXME: consider creating small names
(define (generate-unique-name type-string)
  (string-append type-string 
		 (number->string (generate-unique-number))))

(define identity (lambda (a) a))
