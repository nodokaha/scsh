;; Package definition for the installation library itself.

;; Template for scsh-install-pkg script, must be plugged with the
;; directory containing the Scheme code.
(define scsh-install-pkg-template "#!/bin/sh\nexec scsh -le ~a/load.scm -o install-lib -e install-main -s \"$0\" \"$@\"\n!#")

(define-package "install-lib" (1 3 0)
  ((options (bindir "Destination directory for executables" "<dir>" #t #f
                    ,(bin-dir) ,identity ,identity)))

  ;; Install "package" part (doc & Scheme code)
  (install-file "doc/install-lib.pdf" 'doc "pdf")
  (install-files '("scheme/install-lib.scm"
                   "scheme/install-lib-version.scm"
                   "scheme/install-lib-module.scm")
                 'scheme)
  (write-to-load-script
   `((load ,(absolute-file-name "install-lib-module.scm"
                                (get-directory 'scheme #f)))
     (user)))

  ;; Install "program" part (scsh-install-pkg script).
  (if (phase-active? 'install)
      (let ((bindir (string-append (get-option-value 'dest-dir)
                                   (get-option-value 'bindir))))
        (if (or (get-option-value 'dry-run) (get-option-value 'verbose))
            (format #t "creating scsh-install-pkg script in ~a\n" bindir))

        (if (not (get-option-value 'dry-run))
            (let ((wrapper-name (absolute-file-name "scsh-install-pkg" bindir)))

              (with-errno-handler
               ((errno packet)
                (else ((display-error-and-exit "cannot create wrapper script "
                                               wrapper-name ":\n  "
                                               (first packet)))))

               (create-directory&parents bindir #o755)
               (call-with-output-file wrapper-name
                 (lambda (port)
                   (format port
                           scsh-install-pkg-template
                           (get-directory 'base #f))
                   (set-file-mode port  (integer->file-mode #o755))))))))))
