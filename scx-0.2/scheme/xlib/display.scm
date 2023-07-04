;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

(define-syntax import-xlib-function
  (lambda (exp rename compare)
    (let ((id (cadr exp))
	  (formals (caddr exp))
	  (external-id (cadddr exp))
	  (%define (rename 'define))
	  (%begin (rename 'begin))
	  (%lambda (rename 'lambda))
	  (%binding (rename 'binding))
	  (%import (rename 'import-lambda-definition))
	  (%call-xlib-function (rename 'call-xlib-function)))
      `(,%begin
	(,%import ,%binding ,formals ,external-id)
	(,%define ,id
	  (,%lambda ,formals
	    (,%call-xlib-function ,(car formals) ,id
				  (,%lambda ()
				    (,%binding . ,formals)))))))))

(define (call-xlib-function display name thunk)
  (if (display? display)
      (if (display:warnings? display)
	  (call-critical
	   (lambda ()
	     (let* ((queue (display:error-queue display))
		    (result (thunk)))
	       (if (not (eq? queue (display:error-queue display)))
		   (let* ((next (next-x-error-queue queue))
			  (error (x-error-queue:this next)))
		     (signal-x-warning error)))
	       result)))
	  (thunk))
      (error "first argument of an xlib-function must be a display object"
	     name display)))

(define (call-critical thunk)
  (let ((old-enabled (set-enabled-interrupts! no-interrupts)))
    (with-handler
     (lambda (condition punt)
       (set-enabled-interrupts! old-enabled)
       (punt))
     (lambda ()
       (let ((result (thunk)))
	 (set-enabled-interrupts! old-enabled)
	 result)))))

;; TODO: pixmap-formats (XListPixmapFormats)
(define-record-type display :display
  (make-display cpointer connection-number protocol-version protocol-revision
		server-vendor image-byte-order bitmap-unit bitmap-pad
		bitmap-bit-order vendor-release queue-length name
		default-screen screens after-function wakeup
		warnings? error-queue)
  display?
  (cpointer display:cpointer)
  (connection-number display:connection-number)
  (protocol-version display:protocol-version)
  (protocol-revision display:protocol-revision)
  (server-vendor display:server-vendor)
  (image-byte-order display:image-byte-order)
  (bitmap-unit display:bitmap-unit)
  (bitmap-pad display:bitmap-pad)
  (bitmap-bit-order display:bitmap-bit-order)
  (vendor-release display:vendor-release)
  (queue-length display:queue-length)
  (name display:name)
  (default-screen display:default-screen)
  (screens display:screens)
  (after-function display:after-function set-display:after-function!)
  (wakeup display:wakeup set-display:wakeup!)
  (warnings? display:warnings? set-display:warnings?!)
  (error-queue display:error-queue set-display:error-queue!))

(define-exported-binding "scx-display" :display)

(define (wakeup-display dpy)
  (write-char #\x (cdr (display:wakeup dpy))))

(define (display-wakeup-inport dpy)
  (car (display:wakeup dpy)))

(define (initialize-display dpy)
  (set-display:error-queue! dpy (empty-x-error-queue))
  (call-with-values pipe
		    (lambda (r w)
		      (set-display:wakeup! dpy (cons r w)))))

(define-exported-binding "scx-initialize-display" initialize-display)

(define (display-message-inport display)
  (fdes->inport (display:connection-number display)))

(define-enumerated-type byte-order :byte-order
  byte-order? byte-orders byte-order-name byte-order-index
  (lsb-first msb-first))

(define-exported-binding "scx-byte-order" :byte-order)
(define-exported-binding "scx-byte-orders" byte-orders)

(define-enumerated-type bit-order :bit-order
  bit-order? bit-orders bit-order-name bit-order-index
  (lsb-first msb-first))

(define-exported-binding "scx-bit-order" :bit-order)
(define-exported-binding "scx-bit-orders" bit-orders)

(define-record-type screen-format :screen-format ;; aka pixmap-format
  (make-screen-format depth bits-per-pixel scanline-pad)
  screen-format?
  (depth screen-format:depth)
  (bits-per-pixel screen-format:bits-per-pixel)
  (scanline-pad screen-format:scanline-pad))

(define-exported-binding "scx-screen-format" :screen-format)

(define-record-type screen :screen
  (make-screen cpointer display root-window width height width-mm
	       height-mm number root-depth default-visual default-gc
	       default-colormap white-pixel black-pixel max-maps min-maps
	       does-backing-store does-save-unders? event-mask)
  ;; maybe add depths ?? (TODO)
  ;; does event-mask change ?? (TODO)
  screen?
  (cpointer screen:cpointer)
  (display screen:display)
  (root-window screen:root-window)
  (width screen:width)
  (height screen:height)
  (width-mm screen:width-mm)
  (height-mm screen:height-mm)
  (number screen:number)
  (root-depth screen:root-depth)
  (default-visual screen:default-visual)
  (default-gc screen:default-gc)
  (default-colormap screen:default-colormap)
  (white-pixel screen:white-pixel)
  (black-pixel screen:black-pixel)
  (max-maps screen:max-maps)
  (min-maps screen:min-maps)
  (does-backing-store screen:does-backing-store)
  (does-save-unders? screen:does-save-unders?)
  (event-mask screen:event-mask))

(define-exported-binding "scx-screen" :screen)

;(define (screen:cells screen)
;  (visual:map-entries (screen:default-visual screen)))

(import-lambda-definition screen-number-of-screen (screen)
  "scx_Screen_Number_Of_Screen")

;; *** connect or disconnect to X server *****************************

(import-lambda-definition %open-display (display-name)
  "scx_Open_Display")

;; returns a display or #f
(define (open-display . args)
  (let ((display-name (if (null? args)
			  ""
			  (if (null? (cdr args))
			      (cadr args)
			      (error "invalid arguments" (cdr args))))));; TODO
    (%open-display display-name)))

(import-xlib-function close-display (display)
  "scx_Close_Display")

(define none 0)
(define parent-relative 1)
(define copy-from-parent 0)
(define pointer-window 0)
(define input-focus 1)
(define pointer-root 1)
(define any-property-type 0)
(define any-key 0)
(define all-temporary 0)
(define current-time 0)
(define no-symbol 0)
(define all-planes (- (arithmetic-shift 1 32) 1))

(import-lambda-definition display:last-request-read (display)
  "scx_Display_Last_Request_Read")

;; *** convenience functions *****************************************

(define (default-root-window display)
  (screen:root-window (display:default-screen display)))

(define (black-pixel display)
  (screen:black-pixel (display:default-screen display)))

(define (white-pixel display)
  (screen:white-pixel (display:default-screen display)))

(import-lambda-definition next-request (display)
  "scx_Next_Request")

;; *** enable or disable synchronization *****************************

(define (synchronize dpy on?)
  (if on?
      (set-after-function! dpy
			   (lambda (dpy)
			     (display-sync dpy #f)))
      (set-after-function! dpy #f)))

;; returns the previous after-function. An after-function is called
;; with the display object.

(define (general-after-function display)
  (if (display:after-function display)
      ((display:after-function display) display)
      ;; else the default behaviour ;; TODO: check if this is the real one
      (display-flush display))
  
  ;; if events are in the queue now, then wakeup a wait-event call
  (if (> (events-queued display (queued-mode already)) 0)
      (wakeup-display display)))

(define-exported-binding "scx-general-after-function" general-after-function)

(define (set-after-function! display fun)
  (let ((prev (display:after-function display)))
    (set-display:after-function! display fun)
    prev))

;; *** handle output buffer or event queue ***************************

(import-xlib-function display-flush (display)
  "scx_Display_Flush")

(import-xlib-function display-sync (display discard?)
  "scx_Display_Sync")

;; display-no-op sends a NoOperation protocol request to the X server, thereby
;; exercising the connection. See XNoOp.

(import-xlib-function display-no-op (display)
  "scx_No_Op")

;; *** select input events *******************************************

(import-xlib-function display-select-input (display window event-mask)
  "scx_Display_Select_Input")
