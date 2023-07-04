(define-interface intsets-interface
  (export intset?
          intset-union
          intset-intersection
          intset-difference
          intset-range
          intset-singleton
          intset-adjoin
          intset-delete
          intset-contains?
          intset-map))
(define-structure intsets intsets-interface
  (open scheme
        srfi-23                         ;error
        srfi-1)                         ;list library
  (files intsets))
