#!/bin/sh
exec scsh -lel heap-images/load.scm -lel cml/load.scm -lel scx/load.scm -o xlib -o rendezvous-channels -s "$0" "$@"
!#

(define all-events-mask
  (event-mask
   key-press key-release button-press button-release enter-window leave-window
   pointer-motion pointer-motion-hint button-1-motion button-2-motion 
   button-3-motion button-4-motion button-5-motion button-motion keymap-state
   exposure visibility-change structure-notify resize-redirect
   substructure-notify substructure-redirect focus-change property-change
   colormap-change owner-grab-button))

(define (scxev)
  (let* ((dpy (open-display))
	 (black (black-pixel dpy))
	 (white (white-pixel dpy))
	 (win (create-simple-window dpy (default-root-window dpy) 0 0
				    300 200 0 black white)))

    (set-wm-name! dpy win (string-list->property '("scx Event Listener")))
    (map-window dpy win)

    (init-sync-x-events dpy)
    (call-with-event-channel
     dpy win all-events-mask
     (lambda (channel)
       (let loop ()
	 (let ((e (receive channel)))
	   (display (any-event-type e)) (display " on window ")
	   (display (any-event-window e)) (newline)
	   (if (not (destroy-window-event? e))
	       (loop))))))))

(scxev)
