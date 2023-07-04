(define-interface cavespider-interface
  (export
   ask-server))

(define-structure cavespider 
  cavespider-interface
  (open scheme)
  (files load file-util hash-util html-util string-util util client))
