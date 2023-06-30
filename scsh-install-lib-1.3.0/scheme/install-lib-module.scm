;;; Installation library for scsh modules.
;;; $Id: install-lib-module.scm,v 1.14 2004/11/08 19:54:03 michel-schinz Exp $

;;; Interfaces

(define-interface install-lib-version-interface
  (export install-lib-version))

;; List utilities (extensions to SRFI-1)
(define-interface list-utils-interface
  (export common-prefix-length
          alist-replace
          alist-combine
          alist-get))

;; String utilities (extensions to SRFI-13)
(define-interface string-utils-interface
  (export as-string
          spaces))

;; File utilities (extensions to scsh)
(define-interface file-utils-interface
  (export parent-directory
          create-directory&parents
          relative-file-name
          paths->file-name))

;; Installation library
(define-interface install-interface
  (export tmpl-libtool-la-reader

          version->string
          string->version
          version-compare
          version<?
          version>?
          version=?

          phase-active?

          package-name
          package-full-name
          package-version

          ((define-package define-program) :syntax)
          load-package-in

          install-file
          install-files
          install-directory
          install-directories
          install-directory-contents
          install-string
          install-sub-package

          identity
          parse-boolean
          show-boolean

          get-directory
          get-option-value
          with-output-to-load-script*
          (with-output-to-load-script :syntax)
          write-to-load-script

          display-error-and-exit

          install-main
          install-main-quiet
          install-program-main))

(define-interface install-full-interface
  (compound-interface install-interface
                      list-utils-interface
                      string-utils-interface
                      file-utils-interface))

;;; Structures

(define-structure install-lib-version install-lib-version-interface
  (open scheme)
  (files install-lib-version))

(define-structures ((install-lib install-full-interface)
                    (install install-full-interface)) ;deprecated name
  (open scheme-with-scsh
        cells
        fluids
        let-opt
        srfi-1
        srfi-2
        srfi-9
        srfi-13
        srfi-37
        configure
	posix
        scsh-version
        pp
        install-lib-version)
  (files install-lib))
