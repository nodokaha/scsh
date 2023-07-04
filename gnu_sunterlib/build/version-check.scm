#! /bin/sh
exec scsh -l build/common.scm -l build/dirs.scm -o filenames -s "$0" "$@"
!#

;;; This file is part of the Scheme Untergrund Library. For copyright
;;; information, see the file COPYING which comes with the
;;; distribution.

(load "build/common.scm")
(load "build/dirs.scm")

(define version-re
  (rx "version"
      (+ " ")
      (submatch (+ numeric))
      (* "." (submatch (+ numeric)))))

(define string->version
  (lambda (v)
    (let* ((match (regexp-search version-re v))
           (major (string->number (match:substring match 1)))
           (minor (match:substring match 2)))
      (if minor
          (list major (string->number minor))
          (list major)))))

(define ok '#t)

(define check
  (lambda (dir)
    (call-with-input-file
        (string-append dir "/pkg-def.scm")
      (lambda (port)
        (let try-again ()
          (let ((form (read port)))
            (cond ((eof-object? form)
                   (error "can't find package version"))
                  ((and (pair? form)
                        (eq? (car form) 'define-package))
                   (let ((name (cadr form))
                         (version-pkg-def (caddr form))
                         (version-news
                          (call-with-input-file
                              (string-append dir "/NEWS")
                            (lambda (news-port)
                              (string->version (read-line news-port))))))
                     (if (not (equal? version-pkg-def version-news))
                         (let ()
                           (display "version mismatch in: ")
                           (display name)
                           (newline)
                           (set! ok '#f)))))
                  (else
                   (try-again)))))))))

(for-each check scsh-dirs)
(for-each check s48-dirs)

(exit (if ok 0 1))
