;;; Copyright 2002, 2003 Andreas Bernauer

;;; adapted from Oleg's SXML-tree-trans.scm SRV:send-reply
;;; extended by port argument
;; Displays low-level-sxml on the port. Low-level-sxml contains only
;; strings, characters and thunks. '() and #f are ignored.
(define (display-low-level-sxml fragments port)
  (let loop ((fragments fragments) (result #f))
    (cond
     ((null? fragments) result)
     ((not (car fragments)) (loop (cdr fragments) result))
     ((null? (car fragments)) (loop (cdr fragments) result))
     ((pair? (car fragments))
      (loop (cdr fragments) (loop (car fragments) result)))
     ((procedure? (car fragments))
      ((car fragments))
      (loop (cdr fragments) #t))
     (else
      (display (car fragments) port)
      (loop (cdr fragments) #t)))))

(define sxml->low-level-sxml pre-post-order)

;; Gives you a string representing the HTML of the already reformatted
;; SXML-TREE.
(define (sxml->string sxml-tree rules)
  (call-with-string-output-port
   (lambda (port)
     (display-low-level-sxml 
      (sxml->low-level-sxml sxml-tree rules) 
      port))))

(define (sxml->string/internal sxml-tree rules)
  (list (sxml->string sxml-tree rules)))

;; Predicate for attributes in sxml.
(define (sxml-attribute? thing)
  (and (pair? thing)
       (eq? '@ (car thing))))

;; Returns the attribute list after the initial '@. For our
;; convenience, it ignores values that are not sxml-attributes.
(define (sxml-attribute-attributes thing)
  (if (sxml-attribute? thing)
      (cdr thing)
      '()))

;; Default rule: Creates leading and trailing tag and encloses the
;; attributes.
(define default-rule
  `(*default* 
    . ,(lambda (tag . elems) (apply (entag tag) elems))))

;; Just displays the string, except that some characters are escaped.
(define text-rule
  `(*text*
    . ,(lambda (trigger str) 
	 (if (string? str) (string->goodHTML str) str))))

;; Rule for attribution: creates an attribute like "selected" or
;; "color="red""
(define attribute-rule
  `(@		; local override for attributes
    ((*default*       
      . ,(lambda (attr-key . value) (enattr attr-key value))))
    . ,(lambda (trigger . value) (list '@ value))))

;; Create attribution-value pair for inside of tags
;; If the attribute has no value, value must be '()
(define (enattr attr-key attr-value)
  (if (null? attr-value) 
      (list #\space attr-key)
      (list #\space attr-key "=\"" attr-value #\")))
