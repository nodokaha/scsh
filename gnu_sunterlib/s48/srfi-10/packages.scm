;;; This file is part of the Scheme Untergrund Library.

;; This code, written by Taylor Campbell, is in the public domain.

(define-interface srfi-10-interface
  (export define-reader-constructor))

(define-structure srfi-10 srfi-10-interface
  (open scheme
        reading
        tables
        (subset signals (error)))
  (files srfi-10))

(define-structure srfi-10-test/pi (export)
  (open scheme
        srfi-10
        floatnums)
  (files (test pi)))

(define-structure srfi-10-test/math (export circumference area)
  ;; SRFI-10-TEST/PI already opens SRFI-10, and so the , octothorpe
  ;; syntax is already defined in the reader; it is thus not necessary
  ;; to open SRFI-10 again.
  (open scheme
        srfi-10-test/pi)
  (files (test math)))
