;;; This file is part of the Scheme Untergrund Library.

;; This code, written by Taylor Campbell, is in the public domain.

(define *reader-constructors* (make-symbol-table))
(define (define-reader-constructor name proc)
  (table-set! *reader-constructors* name proc))
(define (reader-constructor name)
  (table-ref  *reader-constructors* name))
(define-sharp-macro #\,
  (lambda (c in)
    (read-char in)
    ;; We want SUB-READ-LIST, not READ, but READING doesn't export it.  Oh
    ;; well: it's just a bit more error checking here...
    (let ((l (read in)))
      (if (and (pair? l) (list? (cdr l)) (symbol? (car l)))
          (cond ((reader-constructor (car l))
                 => (lambda (p) (apply p (cdr l))))
                (else (error "Unrecognized reader constructor" (car l))))
          (error "Invalid #, syntax" l)))))
(define-reader-constructor 'define-reader-constructor
  (lambda (name proc-expression)
    (define-reader-constructor
        (if (symbol? name)
            name
            (error "Bad reader constructor name" name))
      ;; A better version of this package would be integrated with the Scheme48
      ;; module system, with a new kind of clause -- FOR-READER --, with whose
      ;; clauses PROC-EXPRESSION would be evaluated.  Oh well.
      (let ((p (eval proc-expression (interaction-environment))))
        (if (procedure? p)
            p
            (error "Reader constructor expr doesn't evaluate to procedure"
                   name proc-expression p))))
    ;; Must expand at read-time to a valid expression that doesn't really mean
    ;; anything.
    #t))
