;;; This file is part of the Scheme Untergrund Library.

;; This code, written by Taylor Campbell, is in the public domain.

(define (circumference r) (* '#,(pi) 2 r))
(define (area r) (* '#,(pi) (expt r 2)))
