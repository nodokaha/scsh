;; From sunterlib

(define (with-lock* lock thunk)
  (dynamic-wind
   (lambda ()
     (obtain-lock lock))
   thunk
   (lambda ()
     (release-lock lock))))

(define-syntax with-lock
  (syntax-rules ()
    ((with-lock lock body ...)
     (with-lock* lock (lambda () body ...)))))
   