(define-package "heap-images"
  (0 1)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "init.scm" 'scheme))
  (install-file "util.scm" 'scheme))
  (install-file "server.scm" 'scheme))
  (install-file "server-dns.scm" 'scheme))
