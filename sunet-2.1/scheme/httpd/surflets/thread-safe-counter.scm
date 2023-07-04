;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-safe counter

(define-record-type counter :counter
  (really-make-counter counter lock)
  thread-safe-counter?
  (counter counter-counter set-counter-counter!)
  (lock counter-lock))

(define (make-thread-safe-counter)
  (really-make-counter 0 (make-lock)))

;;; read current value
(define (thread-safe-counter-value counter)
  (obtain-lock (counter-lock counter))
  (let ((result (counter-counter counter)))
    (release-lock (counter-lock counter))
    result))

;;; make next value and return it
(define (thread-safe-counter-next! counter)
  (obtain-lock (counter-lock counter))
  (let ((result (+ 1 (counter-counter counter))))
    (set-counter-counter! counter result)
    (release-lock (counter-lock counter))
    result))
