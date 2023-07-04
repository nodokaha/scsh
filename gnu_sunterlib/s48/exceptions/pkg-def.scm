(define-package "exceptions"
  (0 1)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "restart.scm" 'scheme)
  (install-file "srfi-34.scm" 'scheme)
  (install-file "srfi-35.scm" 'scheme))
