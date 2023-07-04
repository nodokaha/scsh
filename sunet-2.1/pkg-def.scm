(define-package "sunet" (2 1) 
  ((install-lib-version (1 0))
   (options (with-surflets "Install with SUrflets (requires SSAX)" "<yes/no>" #t #t #t
                           ,parse-boolean ,show-boolean)))
  (let ((surflets? (get-option-value 'with-surflets)))
    (install-directory-contents "scheme" 'scheme)
    (install-directory "web-server" 'misc-shared)
    (install-directory-contents "doc" 'doc)

    (let ((doc-dir (get-directory 'doc #t))
          (misc-shared-dir (get-directory 'misc-shared #t)))
      (create-symlink (string-append doc-dir "/html")
                      (string-append misc-shared-dir 
                                     "/web-server/root/htdocs/sunet-manual")))
    

    (let ((scheme-dir (get-directory 'scheme #t)))
      (write-to-load-script
       `((user)
         (config)
         (load ,(string-append scheme-dir "/packages.scm"))
         ,@(if surflets?
               `((load ,(string-append scheme-dir "/httpd/surflets/packages.scm")))
               '())
         (user)))))
)
