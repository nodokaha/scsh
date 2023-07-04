(define-package "cml"
  (1 1)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "async-channels.scm" 'scheme)
  (install-file "placeholder.scm" 'scheme)
  (install-file "trans-id.scm" 'scheme)
  (install-file "channel.scm" 'scheme)
  (install-file "jar.scm" 'scheme)
  (install-file "rendezvous.scm" 'scheme))








