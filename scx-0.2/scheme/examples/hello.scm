#!/bin/sh
exec scsh -lel cml/load.scm -lel scx/load.scm -o xlib -o rendezvous-channels -o threads -s "$0" "$@"
!#
; -lel heap-images/load.scm

(define (hello text)
  (let* ((dpy (open-display))
	 (screen (display:default-screen dpy))
	 (cm (screen:default-colormap screen))
	 ;;(cm (copy-colormap-and-free dpy (screen:default-colormap screen)))

	 ;; many ways to get color in your progs.
	 (black (screen:black-pixel screen))
	 (white (screen:white-pixel screen))
	 (blue (alloc-color dpy cm 0 0 1))
	 (green-color (alloc-named-color dpy cm "#00FF00"))
	 (green (if green-color (color:pixel green-color) white))

	 (win (create-simple-window dpy
				    (default-root-window dpy)
				    100 200 400 200 1
				    black white))
	 (gc (create-gc dpy win 
			(make-gc-value-alist (background white)
					     (foreground black))))
	 (font (load-font dpy "*-new century schoolbook-bold-r*24*"))
	 (font2 (load-font dpy "*times*18*")))

    (set-window-colormap! dpy win cm)
    (set-wm-name! dpy win (string-list->property '("scx Hello World Program")))

    (spawn (lambda ()
	     (let loop ((se (most-recent-sync-x-event)))
	       (display "event: ") (display (sync-x-event-event se))
	       (display "\n")
	       (loop (next-sync-x-event se (lambda (e) #t))))))
    ;;(synchronize dpy #f)

    (init-sync-x-events dpy)
    (let ((handler
	   (lambda (channel)
	     (map-window dpy win)
	     (let loop ()
	       (if
		(let ((e (receive channel)))
		  (cond
		   ((expose-event? e)
		    (set-gc-font! dpy gc font)
		    (set-gc-foreground! dpy gc black)
		    (draw-image-string dpy win gc 10 65 text)
		    
		    (set-gc-foreground! dpy gc green)
		    (draw-text dpy win gc 20 40
			       (make-text-items text (change-font font2)
						(with-delta 20 text))))
		   ((motion-event? e) #t)
		   (else #f)))
		(loop))))))
      (call-with-event-channel dpy win (event-mask exposure button-press
						   pointer-motion)
			       handler)
      (close-display dpy))))

(hello "Hello World!")
