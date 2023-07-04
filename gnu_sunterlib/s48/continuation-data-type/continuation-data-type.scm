
;; Taken from Marc Feeley's paper "A Better API for First-Class Continunations"
;; The version from the workshop proceedings contains a bug in continuation-capture.
;; This is the corrected version.

(define (continuation-capture receiver)
  ((call-with-current-continuation
    (lambda (cont)
      (lambda () (receiver cont))))))

(define (continuation-graft cont thunk)
  (cont thunk))

(define (continuation-return cont . returned-values)
  (continuation-graft
   cont
   (lambda () (apply values returned-values))))

