;;; gnuplot.scm -- plotting from Scheme using gnuplot
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Time-stamp: <2000-05-01 emarsden>

(define (open2 cmd)
  (receive (child-read parent-write) (pipe)
  (receive (parent-read child-write) (pipe)
    (cond ((fork)                       ; parent process
           (close child-read)
           (close child-write))
          (else                         ; child process
           (close parent-read)
           (close parent-write)
           (move->fdes child-read 0)
           (move->fdes child-write 1)
           (call-terminally
            (lambda () (run ,((infix-splitter " ") cmd))))))
    (values parent-read parent-write))))

(define (with-gnuplot* thunk)
  (receive (r w)
    (open2 "gnuplot -raise -persist")
    (with-current-output-port w
     (display "set terminal x11\n")
     (display "plot \"-\"\n")
     (thunk))
    (close w)
    (close r)))

(define-syntax with-gnuplot
  (syntax-rules ()
    ((with-gnuplot body ...)
     (with-gnuplot* (lambda () body ...)))))

(define (henon-attractor)
  (do ((x -0.1)
       (y 0.03)
       (temp 0)
       (i 2000 (- i 1)))
      ((zero? i))
    (set! temp x)
    (set! x (+ (* x x -1.4) y 1))
    (set! y (* temp 0.3))
    (format #t "~d ~d\n" x y)))

(with-gnuplot (henon-attractor))
(exit 0)
