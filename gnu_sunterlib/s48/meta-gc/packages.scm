(define-interface meta-gc-interface
  (export
   make-meta make-meta-gc))

(define-structure meta-gc 
  meta-gc-interface
  (open scheme)
  (files gc gc-sized-fast gc-sized meta-gc meta-hook meta stringutil))
