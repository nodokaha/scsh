#! /bin/sh
exec scsh -l build/common.scm -l build/dirs.scm -o filenames -s "$0" "$@"
!#

;;; This file is part of the Scheme Untergrund Library. For copyright
;;; information, see the file COPYING which comes with the
;;; distribution.

;; Create the COPYING file from the AUTHORS files.
(define sunterlib-COPYING
  (lambda ()
    (string-append
     (call-with-input-file "build/header.scm" port->string)
     (apply
      string-append
      (delete-duplicates
       (cons (get-copyrights)
             (map (lambda (dir)
                    (with-cwd dir (get-copyrights)))
                  (append s48-dirs scsh-dirs)))))
     license)))

(with-current-output-port
 (open-output-file "COPYING")
 (display (sunterlib-COPYING)))
