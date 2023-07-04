(define-interface binary-parse-interface
  (export make-bit-reader))

(define-structure binary-parse binary-parse-interface
  (open scheme
        signals
        ascii
        bitwise)
  (files binary-parse))
