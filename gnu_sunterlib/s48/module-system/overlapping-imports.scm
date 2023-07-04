(define (overlapping-imports? forms package)
  (let ((table (make-symbol-table))
	(dups '()))
    (for-each
     (lambda (structure)
       (for-each-export
	(lambda (name want-type binding)
	  (let ((structs (table-ref table name)))
	    (cond ((not structs)
		   (table-set! table name (list (structure-name structure))))
		  ((member (structure-name structure) structs)
		   #f);seems to happen in real life...
		  (else (set! dups (cons name dups))
			(table-set! table
				    name
				    (cons (structure-name structure)
					  (table-ref table name)))))))
	structure))
     (package-opens package))
    (if (not (null? dups))
	(apply warn "package has overlapping imports" 
	       package 
	       (map (lambda (name) (list name (table-ref table name))) dups)))
    forms))
    
(set-optimizer! 'overlapping-imports? overlapping-imports?)
