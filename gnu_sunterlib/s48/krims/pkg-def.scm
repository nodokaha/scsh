(define-package "krims"
  (0 2)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "krims.scm" 'scheme)
  (install-file "onebol.scm" 'scheme)
  (install-file "test.scm" 'scheme))
