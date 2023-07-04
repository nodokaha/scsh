#!/bin/sh
exec scsh -lel heap-images/load.scm -lel cml/load.scm -lel scx/load.scm -o xlib -o rendezvous-channels -o threads -s "$0" "$@"
!#

(define (picture point-count)
  (let* ((dpy (open-display))
	 (width 400)
	 (height 400)
	 (black (black-pixel dpy))
	 (white (white-pixel dpy))
	 (root (default-root-window dpy))
	 (win (create-simple-window dpy root 0 0 width height 1 black white))
	 (gc (create-gc dpy win
			(make-gc-value-alist (background white)
					     (foreground black)))))
    (init-sync-x-events dpy)
    (map-window dpy win)

    (call-with-event-channel
     dpy win (event-mask exposure button-press)
     (lambda (channel)
       (let loop ()
	 (if
	  (let ((e (receive channel)))
	    (cond
	     ((expose-event? e)
	      (clear-window dpy win)
	      (draw-points dpy win gc point-count 0 0 
			   (/ width 2) (/ height 2))
	      (draw-image-string dpy win gc 10 10 "Click a button to exit"))
	     
	     (else #f)))
	  (loop)))))
    (close-display dpy)))

(define (draw-points dpy win gc count x y hw hh)
  (if (zero? (modulo count 100))
      (display-flush dpy))
  (if (not (zero? count))
      (let ((xf (floor (* (+ 1.2 x) hw ))) ; These lines center the picture
	    (yf (floor (* (+ 0.5 y) hh ))))
	(draw-point dpy win gc (inexact->exact xf) (inexact->exact yf))
	(draw-points dpy win gc
		     (- count 1)
		     (- (* y (+ 1 (sin (* 0.7 x))))
			(* 1.2 (sqrt (abs x))))
		     (- 0.21 x)
		     hw hh))))

(picture 1000)
