(define-interface reinitializers-interface
  (export make-reinitializer
          reinitializer?))
  (define-structure reinitializers reinitializers-interface
  (open scheme
        define-record-types
        records)
  (files reinitializer))
