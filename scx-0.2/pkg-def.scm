(define-package "scx" (0 2)
  ((install-lib-version (1 0))
   (options (with-xft "Compile with Xft support" "<yes/no>" #t #t #f
		      ,parse-boolean ,show-boolean)))

  ;; compile and install c libs
  (display "configuring, compiling and installing the native code library\n")
  (let* ((scsh-includes (include-dir))
	 (build-host (get-option-value 'build))
	 (prefix (string-append (get-directory 'lib #f) "/" build-host))
	 (xft? (get-option-value 'with-xft))
         (configure `("./configure"
                      "--enable-static=no"
		      ,(string-append "--prefix=" prefix)
		      ,(if xft? "--with-xft=yes" "--with-xft=no")
		      ,(string-append "--with-scsh-includes=" scsh-includes)
		      ,(string-append "--build=" build-host)))

         (make `(sudo make install
		  ,(string-append "DESTDIR=" (get-option-value 'dest-dir)))))
     (if (get-option-value 'dry-run)
         (begin
          (display configure) (newline)
          (display make) (newline))
         (if (not (and (zero? (run ,configure))
		       (zero? (run ,make))))
	     (exit))))

  ;; create load.scm
  (let ((schemedir (get-directory 'scheme #f))
	(libdir (get-directory 'lib #f))
	(load-xft-packages (get-option-value 'with-xft)))
    (write-to-load-script
     `((new-package)
       (load-package 'dynamic-externals)
       (load-package 'reinitializers)
       (open 'scheme-with-scsh)
       (open 'srfi-13)
       (open 'dynamic-externals)
       (open 'external-calls)
       (open 'configure)
       (open 'signals)
       (open 'reinitializers)
       ,@(map (lambda (x) `(run ',x)) tmpl-libtool-la-reader)
       (run '(define (init-scx)
               (let* ((lib-dir (string-append ,libdir "/" (host)))
		    (la-file-name (string-append lib-dir "/libscx.la"))
		    (initializer-name "scx_init_xlib"))
	       (let ((la-alist (read-libtool-la la-file-name)))
		 (cond
		  ((assoc 'dlname la-alist)
		   => (lambda (p)
			(let ((module-file (string-append lib-dir "/" (cdr p))))
			  (dynamic-load module-file)
                          (lookup-all-externals) ;;; important when resuming images
			  (call-external (get-external initializer-name))
			  ,@(if load-xft-packages
                                '((begin
                                    (call-external (get-external "scx_xft_init"))
                                    (call-external (get-external "scx_xrender_init"))))
                                '()))))
		  (else
		   (error "Could not figure out libscx's name" la-file-name)))))))
       (run '(define scx-reinitializer (make-reinitializer init-scx)))
       (run '(init-scx))
       (config)
       (load ,(string-append schemedir "/xlib/xlib-interfaces.scm"))
       (load ,(string-append schemedir "/xlib/xlib-packages.scm"))
       (load ,(string-append schemedir "/libs/libs-interfaces.scm"))
       (load ,(string-append schemedir "/libs/libs-packages.scm"))
       (user))))

  (install-directory-contents "scheme" 'scheme)
)
