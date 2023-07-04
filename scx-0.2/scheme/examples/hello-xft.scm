#!/bin/sh

exec scsh -lel heap-images/load.scm -lel cml/load.scm -lel scx/load.scm -o xlib -o rendezvous-channels -o threads -o srfi-1 -o signals -o xft -o xrender -s "$0" "$@"
!#

(define *font-size* 36.0)

(define (list-all-fonts display screen)
  (let ((p (make-xft-pattern))
	(os (make-xft-objectset)))
    (xft-objectset-add os (xft-pattern-object family))
    (xft-list-fonts-pattern-objects display screen p os)))

(define (family-name-of-font font)
  (call-with-values
   (lambda ()
     (xft-pattern-get (xft-font-pattern font) (xft-pattern-object family) 0))
   (lambda (code name)
     (if (xft-result-match? code)
	 name "unknown font name"))))

(define (draw-font-name draw color-fg color-bg font)
  (xft-draw-rect draw color-bg 0 0 400 200)
  (xft-draw-string-8bit draw color-fg font 10 65 (family-name-of-font font)))

(define (open-font dpy screen pattern)
  (call-with-values
   (lambda ()
     (let ((copy (xft-pattern-duplicate pattern)))
       (xft-pattern-add copy (xft-pattern-object size) *font-size* #f)
       (xft-font-match dpy screen copy)))
   (lambda (result pattern)
     (cond 
      ((xft-font-open-pattern dpy pattern)
       => (lambda (font) font))
      (else
       (xft-pattern-print pattern)
       (error "Could not open font!"))))))
 
(define (font-demo)
  (let* ((dpy (open-display))
	 (screen (display:default-screen dpy))
	 (cm (screen:default-colormap screen))
	 (cm (copy-colormap-and-free dpy (screen:default-colormap screen)))

	 ;; many ways to get color in your progs.
	 (black (screen:black-pixel screen))
	 (white (screen:white-pixel screen))

	 (win (create-simple-window dpy
				    (default-root-window dpy)
				    100 200 400 200 1
				    black white))
	 (gc (create-gc dpy win 
			(make-gc-value-alist (background white)
					     (foreground black))))
	 (visual (screen:default-visual screen))
	 (xft-draw (make-xft-draw dpy win visual cm))
	 (xft-black (xft-color-alloc-name dpy visual cm "black"))
	 (xft-white (xft-color-alloc-name dpy visual cm "white"))
	 (standard-font (open-font dpy screen (make-xft-pattern))))

    (set-window-colormap! dpy win cm)
    (set-wm-name! dpy win (string-list->property '("scx Xft Demo Program")))

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
	     (let loop ((font-patterns (list-all-fonts dpy screen))
			(font standard-font))

	       (if
		(let ((e (receive channel)))
		  (cond

		   ((button-event? e)
		    (if (null? font-patterns)
			(loop (list-all-fonts dpy screen) standard-font)
			(let ((font (open-font dpy screen (car font-patterns))))
			  (xft-pattern-print (car font-patterns))
			  (draw-font-name xft-draw xft-black xft-white font)
			  (loop (cdr font-patterns) font))))

		   ((expose-event? e)
		    (draw-font-name xft-draw xft-black xft-white font))

		   ((motion-event? e) #t)
		   (else #f)))
		(loop font-patterns font))))))
      (call-with-event-channel dpy win (event-mask exposure button-press
						   pointer-motion)
			       handler)
      (close-display dpy))))

(font-demo)
