;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (with-lock lock thunk)
  (dynamic-wind 
   (lambda ()
     (obtain-lock lock))
   thunk
   (lambda ()
     (release-lock lock))))
