;;; weak table with weak values

(define-record-type weak-table
  (really-make-weak-table table)
  weak-table?
  (table weak-table-table))

(define (make-value-weak-table)
  (really-make-weak-table (make-integer-table)))

(define (add-to-weak-table! table address object)
  (table-set! (weak-table-table table)
	      address 
	      (make-weak-pointer object)))

(define (remove-from-weak-table! table address)
  (table-set! (weak-table-table table)
	      address #f))

(define (lookup-in-weak-table table address)
  (cond
   ((table-ref (weak-table-table table) address)
    => weak-pointer-ref)
   (else #f)))
