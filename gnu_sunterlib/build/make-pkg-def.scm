#! /bin/sh
exec scsh -l build/common.scm -l build/dirs.scm -o filenames -s "$0" "$@"
!#

;;; This file is part of the Scheme Untergrund Library. For copyright
;;; information, see the file COPYING which comes with the
;;; distribution.

(define version (map string->number command-line-arguments))

(define entry
  (lambda (dir)
    (display "(load-package-in \"")
    (display dir)
    (display "\")\n")))

(with-current-output-port
 (open-output-file "pkg-def.scm")
 (display (call-with-input-file "build/header.scm" port->string))
 (display (call-with-input-file "build/common.scm" port->string))
 (display "\n(define-package \"sunterlib\"\n  ")
 (display version)
 (display "\n  ()
  (install-file \"COPYING\" 'doc)
  (install-file \"DETAILS\" 'doc)
  (install-file \"NEWS\" 'doc)
  (install-file \"README\" 'doc)
  (install-file \"README.contrib\" 'doc))\n")
 (display "\n;; S48 LIBRARIES\n\n")
 (for-each entry s48-dirs)
 (display "\n;; SCSH LIBRARIES\n\n")
 (for-each entry scsh-dirs))
