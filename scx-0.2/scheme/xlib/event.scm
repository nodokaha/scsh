;; Thread-safe event reading *****************************************

;; wait-event blocks the current thread until an event is available,
;; and then it returns this new event.

(define (wait-event dpy)
  (if (> (events-queued dpy (queued-mode after-flush)) 0)
      (next-event dpy)
      (begin
	(really-wait-event dpy)
	(wait-event dpy))))

(define (really-wait-event dpy . maybe-timeout)
  ;; selects on the port to the X-server and on the internal wakeup
  ;; pipe. We get woke up, if a Xlib-call reads events and puts them
  ;; in the Xlib-internal event queue in our back. See
  ;; general-after-function.
  (let* ((message-port (display-message-inport dpy))
	 (wakeup-port (display-wakeup-inport dpy))
	 (l (select-port-channels (if (null? maybe-timeout)
				      #f
				      (car maybe-timeout))
				  message-port
				  wakeup-port)))
    ;; read all characters from the wakeup-port
    (if (member wakeup-port l)
	(let loop ()
	  (if (char-ready? wakeup-port)
	      (begin
		(read-char wakeup-port)
		(loop)))))
    (member message-port l)))

;; How to find out if there are events available *********************

(define-enumerated-type queued-mode :queued-mode
  queued-mode? queued-modes queued-mode-name queued-mode-index
  (already after-reading after-flush))

(define-exported-binding "scx-queued-mode" :queued-mode)

(import-xlib-function events-queued (display mode)
  "scx_Events_Queued")

(define (event-ready? display)
  (or (> (events-queued display (queued-mode already)) 0)
      (char-ready? (display-message-inport display))))

;; events-pending is identical to events-queued with after-flush
;; mode.

(import-xlib-function events-pending (display)
  "scx_Events_Pending")

;; Other event reading ***********************************************

(import-xlib-function next-event (display)
  "scx_Next_Event")

(import-xlib-function peek-event (display)
  "scx_Peek_Event")

;; returns a list of (time . (x . y)) elements
(import-xlib-function get-motion-events (display window from to)
  "scx_Get_Motion_Events")

;; Sending events ****************************************************

(import-xlib-function send-event (display window propagate mask event)
  "scx_Send_Event")
