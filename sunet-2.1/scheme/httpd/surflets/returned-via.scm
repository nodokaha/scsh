
(define (returned-via return-object bindings)
  (if (input-field? return-object)
      (input-field-value return-object bindings)
      ;; We assume we have a return-address-object instead.
      (let ((address (return-object 'address)))
	(cond 
	 ((assoc (address-name address) bindings) =>
	  (lambda (pair)
	    (if (address-annotated? address)
		(address-annotation address (cdr pair))
		#t)))
	 (else #f)))))

;; It depends on the object, if returned-via returns only boolean
;; values or string values as well. So let us have both names.
(define returned-via? returned-via)

;; This is from Martin Gasbichler
(define-syntax case-returned-via
  (syntax-rules (else =>)
    ((case-returned-via (%bindings ...) clauses ...)
     (let ((bindings (%bindings ...)))
       (case-returned-via bindings clauses ...)))
    ((case-returned-via bindings (else body ...))
     (begin body ...))
    ((case-returned-via bindings
                        ((%return-object ...) => %proc))
     (cond ((or (returned-via %return-object bindings) ...)
            => %proc)))
    ((case-returned-via bindings
                        ((%return-object ...) %body ...))
     (if (or (returned-via? %return-object bindings) ...)
         (begin %body ...)))
    ((case-returned-via bindings
                        ((%return-object ...) => %proc)
                        %clause %clauses ...)
     (cond ((or (returned-via %return-object bindings) ...)
            => %proc)
           (else
            (case-returned-via bindings %clause %clauses ...))))
    ((case-returned-via bindings
                        ((%return-object ...) %body ...)
                        %clause %clauses ...)
     (if (or (returned-via? %return-object bindings) ...)
         (begin %body ...) 
         (case-returned-via bindings %clause %clauses ...)))))