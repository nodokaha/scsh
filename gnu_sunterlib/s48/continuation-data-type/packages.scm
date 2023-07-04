(define-interface continuation-data-type-interface
  (export continuation-capture
	  continuation-graft
	  continuation-return))
(define-structure continuation-data-type continuation-data-type-interface
  (open scheme)
  (files continuation-data-type))