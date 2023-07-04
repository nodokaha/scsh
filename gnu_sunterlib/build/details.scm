#! /bin/sh
exec scsh -l build/dirs.scm -o filenames -s "$0" "$@"
!#

;;; This file is part of the Scheme Untergrund Library. For copyright
;;; information, see the file COPYING which comes with the
;;; distribution.

(define entry
  (lambda (dir)
    (with-current-input-port
     (open-input-file (string-append dir "/BLURB"))
     (let loop ()
       (let ((ch (read-char)))
         (if (eof-object? ch)
             (values)
             (begin
               (write-char ch)
               (loop))))))
    (with-current-input-port
     (open-input-file (string-append dir "/AUTHORS"))
     (let loop ()
       (let ((ch (read-char)))
         (if (eof-object? ch)
             (values)
             (begin
               (write-char ch)
               (loop))))))
    (newline)))

;; Create the details file from the AUTHORS and BLURB files.
(with-current-output-port
 (open-output-file "DETAILS")
 (display (call-with-input-file "build/header.scm" port->string))
 (display "S48 LIBRARIES\n\n")
 (for-each entry s48-dirs)
 (display "SCSH LIBRARIES\n\n")
 (for-each entry scsh-dirs))
