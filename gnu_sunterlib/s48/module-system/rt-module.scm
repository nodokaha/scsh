;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (interface-value-names interface-name)
  (let ((interface (environment-ref (config-package) interface-name))
	(value-names '()))
    (for-each-declaration
     (lambda (name base-neme type)
       (if (not (equal? type syntax-type))
	   (set! value-names (cons name value-names))))
     interface)
    value-names))

(define-record-type rt-structure :rt-structure
  (make-rt-structure meta-structure)
  rt-structure?
  (meta-structure rt-structure-meta-structure))

(define (rt-structure-loaded? rt-structure)
  (package-loaded? 
   (structure-package (rt-structure-meta-structure rt-structure))))

(define-record-discloser :rt-structure
  (lambda (s)
    (list 'rt-stucture (structure-name (rt-structure-meta-structure s)))))

(define (reify-structure name)
  (let ((struct (get-structure name)))
    (make-rt-structure struct)))

(define (load-structure rts)
  (ensure-loaded (rt-structure-meta-structure rts)))

(define (rt-structure-binding structure name)
  (if (not (rt-structure-loaded? structure))
      (error "Structure not loaded" structure))
  (contents
   (binding-place
    (generic-lookup (rt-structure-meta-structure structure)
		    name))))

(define (rt-structure->environment rt-structure)
  (structure-package (rt-structure-meta-structure rt-structure)))

(define (load-config-file file)
  (load file (config-package)))


	  
