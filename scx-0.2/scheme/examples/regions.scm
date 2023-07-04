#!/bin/sh
exec scsh -lel heap-images/load.scm -lel cml/load.scm -lel scx/load.scm -o xlib -o rendezvous-channels -s "$0" "$@"
!#

(define (regions)
  (let* ((dpy (open-display))
	 (cm (screen:default-colormap (display:default-screen dpy)))
	 (black (black-pixel dpy))
	 (white (white-pixel dpy))
	 (blue (color:pixel (alloc-named-color dpy cm "blue")))
	 (win (create-simple-window dpy (default-root-window dpy)
				    0 0 500 500 1
				    black white))
	 (gc (create-gc dpy win
			(make-gc-value-alist
			 (background white)
			 (foreground black))))
	 
	 (rectangles (list (make-rectangle 10 20 60 60)
			   (make-rectangle 50 100 30 30)))
	 (colors (list black blue))
	 (regions-alist
	  (map (lambda (rect text)
		 (cons (union-rect-with-region (rectangle:x rect)
					       (rectangle:y rect)
					       (rectangle:width rect)
					       (rectangle:height rect)
					       (create-region))
		       text))
	       rectangles
	       '("black rectangle" "blue rectangle"))))

    (map-window dpy win)
    (init-sync-x-events dpy)

    (call-with-event-channel
     dpy win (event-mask exposure button-press structure-notify)
     (lambda (channel)
       (let loop ()
	 (let ((e (receive channel)))
	   (cond
	    ;; Zeichnen...
	    ((expose-event? e)
	     (for-each (lambda (rect color)
			 (set-gc-foreground! dpy gc color)
			 (fill-rectangles dpy win gc (list rect)))
		       rectangles colors))
	    ;; Hit-Tests
	    ((button-event? e)
	     (let* ((x (button-event-x e))
		    (y (button-event-y e))
		    (rs (filter (lambda (r-n)
				  (point-in-region? (car r-n)
						    x y))
				regions-alist)))
	       (for-each (lambda (region-name)
			   (display "You clicked: ")
			   (display (cdr region-name))
			   (newline))
			 rs)
	       ;; break if none was hit.
	       (if (null? rs)
		   (begin
		     (close-display dpy)
		     (exit)))))))
	 (loop))))))

(regions)
