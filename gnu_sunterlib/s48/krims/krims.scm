; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library
; See README for documentation.

(define-syntax assert
      (syntax-rules ()
	((assert ?x ?y0 ...)
	 (if (not ?x) (error "Assertion failed" '?x ?y0 ...))) ))


;; RECEIVE/NAME is a multiple values analogue of named LET. 
(define-syntax receive/name
  (syntax-rules ()
    ((_ ?tag ?tuple ?call ?body0 ?body1 ...)
     (letrec ((proc
               (lambda ?tuple
                 (let-syntax
                     ((?tag (syntax-rules ()
                              ((?tag ?e)
                               (call-with-values (lambda () ?e)
                                 (lambda ?tuple (proc . ?tuple))))
                              ((?tag . ?args)
                               (proc . ?args)))))
                   ?body0 ?body1 ...))))
       (call-with-values (lambda () ?call) proc)))))


;; dispatch on type of the first argument
;; [ should we support a default clause (else ?proc) ? ]
(define-syntax gen-dispatch
  (syntax-rules ()
    ((_ () ?x0 . ?rest)
     #f)
    ((_ ((?pred ?proc) ...) ?x0 . ?rest)
     (cond ((?pred ?x0) (?proc ?x0 . ?rest))
           ...
           (else (error "unsupported input type" ?x0))))))
