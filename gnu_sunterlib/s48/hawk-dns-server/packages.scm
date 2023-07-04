(define-interface hawk-dns-server-interface
  (export
   make-server
   make-server-dns))

(define-structure 
  hawk-dns-server-interface
  (open scheme)
  (files load util server db server-dns))
