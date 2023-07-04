;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

(define-enumerated-type bit-gravity :bit-gravity
  bit-gravity? bit-gravities bit-gravity-name bit-gravity-index
  (forget north-west north north-east west center east south-west
   south south-east static))

(define-exported-binding "scx-bit-gravity" :bit-gravity)
(define-exported-binding "scx-bit-gravities" bit-gravities)

(define-enumerated-type win-gravity :win-gravity
  win-gravity? win-gravities win-gravity-name win-gravity-index
  (unmap north-west north north-east west center east south-west
   south south-east static))

(define-exported-binding "scx-win-gravity" :win-gravity)
(define-exported-binding "scx-win-gravities" win-gravities)

(define-enumerated-type backing-store :backing-store
  backing-store? backing-stores backing-store-name backing-store-index
  (not-useful when-mapped always))

(define-exported-binding "scx-backing-store" :backing-store)
(define-exported-binding "scx-backing-stores" backing-stores)

(define-enumerated-type set-window-attribute :set-window-attribute
  set-window-attribute?
  set-window-attributes
  set-window-attribute-name
  set-window-attribute-index
  ;; don't change the order of the attributes!  background-pixmap can
  ;; be a pixmap including (special-pixmap:none dpy) and
  ;; (special-pixmap:parent-relative dpy) border-pixmap can be a
  ;; pixmap or (special-pixmap:copy-from-parent dpy)
  (background-pixmap background-pixel border-pixmap border-pixel
   bit-gravity gravity backing-store backing-planes backing-pixel
   override-redirect save-under event-mask do-not-propagate-mask colormap
   cursor))

(define-exported-binding "scx-set-window-attribute" :set-window-attribute)

(define-syntax make-set-window-attribute-alist
  (syntax-rules 
   ()
   ((make-set-window-attribute-alist (attr arg) rest ...)
    (cons (cons (set-window-attribute attr) arg)
	  (make-set-window-attribute-alist rest ...)))
   ((make-set-window-attribute-alist)
    '())))

;; *** create windows ************************************************

(import-xlib-function create-window 
  (display parent x y width height border_width depth class visual attribs)
  "scx_Create_Window")

(import-xlib-function create-simple-window 
  (display parent x y width height border_width border background)
  "scx_Create_Simple_Window")

;; *** change window attributes **************************************

(import-xlib-function change-window-attributes (display window attribs)
  "scx_Change_Window_Attributes")

(define (make-win-attr-setter attribute)
  (lambda (display window value)
    (change-window-attributes display window (list (cons attribute value)))))

(define set-window-background-pixmap! 
  (make-win-attr-setter (set-window-attribute background-pixmap)))
(define set-window-background-pixel! 
  (make-win-attr-setter (set-window-attribute background-pixel)))
(define set-window-border-pixmap! 
  (make-win-attr-setter (set-window-attribute border-pixmap)))
(define set-window-border-pixel! 
  (make-win-attr-setter (set-window-attribute border-pixel)))
(define set-window-bit-gravity! 
  (make-win-attr-setter (set-window-attribute bit-gravity)))
(define set-window-gravity! 
  (make-win-attr-setter (set-window-attribute gravity)))
(define set-window-backing-store! 
  (make-win-attr-setter (set-window-attribute backing-store)))
(define set-window-backing-planes! 
  (make-win-attr-setter (set-window-attribute backing-planes)))
(define set-window-backing-pixel! 
  (make-win-attr-setter (set-window-attribute backing-pixel)))
(define set-window-save-under! 
  (make-win-attr-setter (set-window-attribute save-under)))
(define set-window-event-mask! 
  (make-win-attr-setter (set-window-attribute event-mask)))
(define set-window-do-not-propagate-mask! 
  (make-win-attr-setter (set-window-attribute do-not-propagate-mask)))
(define set-window-override-redirect! 
  (make-win-attr-setter (set-window-attribute override-redirect)))
(define set-window-colormap! 
  (make-win-attr-setter (set-window-attribute colormap)))
(define set-window-cursor! 
  (make-win-attr-setter (set-window-attribute cursor)))

;; *** configure windows *********************************************

(define-enumerated-type stack-mode :stack-mode
  stack-mode? stack-modes stack-mode-name stack-mode-index
  (above below top-if buttom-if opposite))

(define-exported-binding "scx-stack-mode" :stack-mode)
(define-exported-binding "scx-stack-modes" stack-modes)

;; an enumerated type for XWindowChange. Used in configure-window

(define-enumerated-type window-change :window-change
  window-change? window-changes window-change-name window-change-index
  (x y width height border-width sibling stack-mode))

(define-exported-binding "scx-window-change" :window-change)
(define-exported-binding "scx-window-changes" window-changes)

(define-syntax make-window-change-alist
  (syntax-rules 
   ()
   ((make-window-change-alist (attr arg) rest ...)
    (cons (cons (window-change attr) arg)
	  (make-window-change-alist rest ...)))
   ((make-window-change-alist)
    '())))

(import-xlib-function configure-window (display window changes)
  "scx_Configure_Window")

(define (make-win-configurer change)
  (lambda (display window value)
    (configure-window display window (list (cons change value)))))

(define set-window-x! (make-win-configurer (window-change x)))
(define set-window-y! (make-win-configurer (window-change y)))
(define set-window-width! (make-win-configurer (window-change width)))
(define set-window-height! (make-win-configurer (window-change height)))
(define set-window-border-width! 
  (make-win-configurer (window-change border-width)))
(define set-window-sibling! (make-win-configurer (window-change sibling)))
(define set-window-stack-mode! 
  (make-win-configurer (window-change stack-mode)))

(define (move-window display window x y)
  (configure-window display window
		    (make-window-change-alist (x x) (y y))))

(define (resize-window display window width height)
  (configure-window display window
		    (make-window-change-alist (width width)
					      (height height))))

(define (move-resize-window display window x y width height)
  (configure-window display window
		    (make-window-change-alist (x x) (y y)
					      (width width)
					      (height height))))

;; *** get current window attribute or geometry **********************

(define-enumerated-type map-state :map-state
  map-state? map-states map-state-name map-state-index
  (is-unmapped is-unviewable is-viewable))

(define-exported-binding "scx-map-state" :map-state)
(define-exported-binding "scx-map-states" map-states)

(define-enumerated-type window-class :window-class
  window-class? window-classes window-class-name window-class-index
  (copy-from-parent input-output input-only))

(define-exported-binding "scx-window-class" :window-class)
(define-exported-binding "scx-window-classes" window-classes)

(define-record-type window-attributes :window-attributes
  (make-window-attributes x y width height border-width depth visual root
			  class bit-gravity gravity backing-store
			  backing-planes backing-pixel save-under
			  colormap map-installed map-state all-event-masks
			  your-event-mask do-not-propagate-mask
			  override-redirect screen)
  window-attributes?
  (x window-attribute:x)
  (y window-attribute:y)
  (width window-attribute:width)
  (height window-attribute:height)
  (border-width window-attribute:border-width)
  (depth window-attribute:depth)
  (visual window-attribute:visual)
  (root window-attribute:root)
  (class window-attribute:class)
  (bit-gravity window-attribute:bit-gravity)
  (gravity window-attribute:gravity)
  (backing-store window-attribute:backing-store)
  (backing-planes window-attribute:backing-planes)
  (backing-pixel window-attribute:backing-pixel)
  (save-under window-attribute:save-under)
  (colormap window-attribute:colormap)
  (map-installed window-attribute:map-installed)
  (map-state window-attribute:map-state)
  (all-event-masks window-attribute:all-event-masks)
  (your-event-mask window-attribute:your-event-mask)
  (do-not-propagate-mask window-attribute:do-not-propagate-mask)
  (override-redirect window-attribute:override-redirect)
  (screen window-attribute:screen))

(define-exported-binding "scx-window-attributes" :window-attributes)

(import-xlib-function get-window-attributes (display window)
  "scx_Get_Window_Attributes")

;; returns a vector #(root-window x y width height border-width depth) or #f
(import-xlib-function get-geometry (display drawable)
  "scx_Get_Geometry")

(define (make-geometry-getter i)
  (lambda (display window)
    (let ((a (get-geometry display window)))
      (and a (vector-ref a i)))))

;;(define window-root (make-geometry-getter 0))
(define window-x (make-geometry-getter 1))
(define window-y (make-geometry-getter 2))
(define window-width (make-geometry-getter 3))
(define window-height (make-geometry-getter 4))
(define window-border-width (make-geometry-getter 5))
(define window-depth (make-geometry-getter 6))

;; *** map windows ***************************************************

(import-xlib-function map-window (display window)
  "scx_Map_Window")

(import-xlib-function map-raised (display window)
  "scx_Map_Raised")

(import-xlib-function map-subwindows (display window)
  "scx_Map_Subwindows")

;; *** unmap windows *************************************************

(import-xlib-function unmap-window (display window)
  "scx_Unmap_Window")

(import-xlib-function unmap-subwindows (display window)
  "scx_Unmap_Subwindows")

;; *** destroy windows ***********************************************

(import-xlib-function destroy-window (display window)
  "scx_Destroy_Window")

(import-xlib-function destroy-subwindows (display window)
  "scx_Destroy_Subwindows")

;; *** change window stacking order **********************************

(import-xlib-function raise-window (display window)
  "scx_Raise_Window")

(import-xlib-function lower-window (display window)
  "scx_Lower_Window")

(define-enumerated-type circulate-direction :circulate-direction
  circulate-direction? circulate-directions circulate-direction-name
  circulate-direction-index
  (raise-lowest lower-highest))

(define-exported-binding "scx-circulate-direction" :circulate-direction)

(import-xlib-function circulate-subwindows (display window direction)
  "scx_Circulate_Subwindows")

(define (circulate-subwindows-up display window)
  (circulate-subwindows display window (circulate-direction raise-lowest)))

(define (circulate-subwindows-down display window)
  (circulate-subwindows display window (circulate-direction lower-highest)))

(import-xlib-function restack-windows (display windows)
  "scx_Restack_Windows")

;; *** clear area or window ******************************************

(import-xlib-function clear-area
  (display window x y width height exposures?)
  "scx_Clear_Area")

(import-xlib-function clear-window (display window)
  "scx_Clear_Window")

;; *** query window tree information *********************************

;; returns a list (root-window parent-window children) or #f
(import-xlib-function query-tree (display window)
  "scx_Query_Tree")

(define (window-root display window)
  (let ((t (query-tree display window)))
    (and t (car t))))

(define (window-parent display window)
  (let ((t (query-tree display window)))
    (and t (cadr t))))

(define (window-children display window)
  (let ((t (query-tree display window)))
    (and t (caddr t))))

;; *** translate window coordinates **********************************

;; returns a list (dest-x dest-y child) or #f
(import-xlib-function translate-coordinates
  (display src-w dest-w src-x src-y)
  "scx_Translate_Coordinates")

;; *** get pointer coordinates ***************************************

(import-xlib-function %query-pointer (display window)
  "scx_Query_Pointer")

(define (query-pointer-root display)
  (let ((q (%query-pointer display (default-root-window display))))
    (and q (list (vector-ref q 0)     ;; the root-window that the pointer is on
		 (vector-ref q 2)     ;; x and
		 (vector-ref q 3))))) ;; y coordinates on that root-window

(define (query-pointer-state display)
  (let ((q (%query-pointer display (default-root-window display))))
    (and q (vector-ref q 6))))

(define (query-pointer display window)
  (let ((q (%query-pointer display window)))
    (and q (vector-ref q 7)
	 (list (vector-ref q 1)     ;; child of window that contains
				    ;; the pointer or None
	       (vector-ref q 4)     ;; x and y coordinates
	       (vector-ref q 5))))) ;; relative to window

;; *** convenience functions *****************************************

(define (window-exists? dpy window)
  (display-sync dpy #f)
  (let ((before (use-x-error-warnings! dpy #t)))
    (let ((result
	   (call-with-current-continuation
	    (lambda (return)
	      (with-handler (lambda (condition punt)
			      (return #f))
			    (lambda ()
			      (query-tree dpy window)
			      (display-sync dpy #f)
			      #t))))))
      (if (not before) (use-x-error-warnings! dpy #f))
      result)))
