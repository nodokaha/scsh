;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2003 by Taylor Campbell
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define make-procedure-table
  (make-table-maker eq? procedure-hash))
