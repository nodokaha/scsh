(define-interface srfi-37-interface
  (export
   option
   option?
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor
   args-fold))

(define-structure srfi-37
  srfi-37-interface
  (open scheme srfi-9 srfi-11)
  (files args-fold))
