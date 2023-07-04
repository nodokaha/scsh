;; An enumerated type for event types. They correspond to the
;; constants defined in X.h. So don't change the order!

(define-enumerated-type event-type :event-type
  event-type?
  event-types
  event-type-name
  event-type-index
  (event-type-0 event-type-1 ;; those are not defined
   key-press key-release button-press button-release motion-notify
   enter-notify leave-notify focus-in focus-out keymap-notify expose
   graphics-expose no-expose visibility-notify create-notify destroy-notify
   unmap-notify map-notify map-request reparent-notify configure-notify
   configure-request gravity-notify resize-request circulate-notify
   circulate-request property-notify selection-clear selection-request
   selection-notify colormap-notify client-message mapping-notify))

(define-exported-binding "scx-event-type" :event-type)
(define-exported-binding "scx-event-types" event-types)

;; *******************************************************************

(define-enumerated-type notify-mode :notify-mode
  notify-mode? notify-modes notify-mode-name notify-mode-index
  (normal grab ungrab while-grabbed))

(define-exported-binding "scx-notify-mode" :notify-mode)
(define-exported-binding "scx-notify-modes" notify-modes)

;; *******************************************************************

(define-enumerated-type notify-detail :notify-detail
  notify-detail? notify-details notify-detail-name notify-detail-index
  (ancestor virtual inferior nonlinear nonlinear-virtual pointer
   pointer-root detail-none))

(define-exported-binding "scx-notify-detail" :notify-detail)
(define-exported-binding "scx-notify-details" notify-details)

;; *******************************************************************

(define-enumerated-type visibility-state :visibility-state
  visibility-state? visibility-states visibility-state-name 
  visibility-state-index
  (unobscured partially-obscured fully-obscured))

(define-exported-binding "scx-visibility-state" :visibility-state)
(define-exported-binding "scx-visibility-states" visibility-states)

;; *******************************************************************

(define-enumerated-type place :place
  place? places place-name place-index
  (on-top on-bottom))

(define-exported-binding "scx-place" :place)
(define-exported-binding "scx-places" places)

;; *******************************************************************

(define-enumerated-type property-state :property-state
  property-state? property-states property-state-name property-state-index
  (new-value delete))

(define-exported-binding "scx-property-state" :property-state)
(define-exported-binding "scx-property-states" property-states)

(define-enumerated-type property-format :property-format
  property-format? property-formats property-format-name property-format-index
  (char short long))

(define-exported-binding "scx-property-format" :property-format)
(define-exported-binding "scx-property-formats" property-formats)

;; *******************************************************************

(define-enumerated-type mapping-request :mapping-request
  mapping-request? mapping-requests mapping-request-name mapping-request-index
  (modifier keyboard pointer))

(define-exported-binding "scx-mapping-request" :mapping-request)
(define-exported-binding "scx-mapping-requests" mapping-requests)

;; *******************************************************************

(define-enumerated-type event-mask-item :event-mask-item
  event-mask-item? event-mask-items event-mask-item-name event-mask-item-index
  (key-press key-release button-press button-release enter-window leave-window
   pointer-motion pointer-motion-hint button-1-motion button-2-motion 
   button-3-motion button-4-motion button-5-motion button-motion keymap-state
   exposure visibility-change structure-notify resize-redirect
   substructure-notify substructure-redirect focus-change property-change
   colormap-change owner-grab-button))

(define-enum-set-type event-mask :event-mask
  event-mask? make-event-mask
  event-mask-item event-mask-item? event-mask-items event-mask-item-index)

(define-exported-binding "scx-event-mask" :event-mask)

(define (any-event-type event)
  (let ((f
	 (cond
	  ((key-event? event) key-event-type)
	  ((button-event? event) button-event-type)
	  ((motion-event? event) motion-event-type)
	  ((crossing-event? event) crossing-event-type)
	  ((focus-change-event? event) focus-change-event-type)
	  ((expose-event? event) expose-event-type)
	  ((graphics-expose-event? event) graphics-expose-event-type)
	  ((no-expose-event? event) no-expose-event-type)
	  ((visibility-event? event) visibility-event-type)
	  ((create-window-event? event) create-window-event-type)
	  ((destroy-window-event? event) destroy-window-event-type)
	  ((unmap-event? event) unmap-event-type)
	  ((map-event? event) map-event-type)
	  ((map-request-event? event) map-request-event-type)
	  ((reparent-event? event) reparent-event-type)
	  ((configure-event? event) configure-event-type)
	  ((gravity-event? event) gravity-event-type)
	  ((resize-request-event? event) resize-request-event-type)
	  ((configure-request-event? event) configure-request-event-type)
	  ((circulate-event? event) circulate-event-type)
	  ((circulate-request-event? event) circulate-request-event-type)
	  ((property-event? event) property-event-type)
	  ((selection-clear-event? event) selection-clear-event-type)
	  ((selection-request-event? event) selection-request-event-type)
	  ((selection-event? event) selection-event-type)
	  ((colormap-event? event) colormap-event-type)
	  ((client-message-event? event) client-message-event-type)
	  ((mapping-event? event) mapping-event-type)
	  ((keymap-event? event) keymap-event-type))))
    (f event)))

(define (any-event-serial event)
  (let ((f
	 (cond
	  ((key-event? event) key-event-serial)
	  ((button-event? event) button-event-serial)
	  ((motion-event? event) motion-event-serial)
	  ((crossing-event? event) crossing-event-serial)
	  ((focus-change-event? event) focus-change-event-serial)
	  ((expose-event? event) expose-event-serial)
	  ((graphics-expose-event? event) graphics-expose-event-serial)
	  ((no-expose-event? event) no-expose-event-serial)
	  ((visibility-event? event) visibility-event-serial)
	  ((create-window-event? event) create-window-event-serial)
	  ((destroy-window-event? event) destroy-window-event-serial)
	  ((unmap-event? event) unmap-event-serial)
	  ((map-event? event) map-event-serial)
	  ((map-request-event? event) map-request-event-serial)
	  ((reparent-event? event) reparent-event-serial)
	  ((configure-event? event) configure-event-serial)
	  ((gravity-event? event) gravity-event-serial)
	  ((resize-request-event? event) resize-request-event-serial)
	  ((configure-request-event? event) configure-request-event-serial)
	  ((circulate-event? event) circulate-event-serial)
	  ((circulate-request-event? event) circulate-request-event-serial)
	  ((property-event? event) property-event-serial)
	  ((selection-clear-event? event) selection-clear-event-serial)
	  ((selection-request-event? event) selection-request-event-serial)
	  ((selection-event? event) selection-event-serial)
	  ((colormap-event? event) colormap-event-serial)
	  ((client-message-event? event) client-message-event-serial)
	  ((mapping-event? event) mapping-event-serial)
	  ((keymap-event? event) keymap-event-serial))))
    (f event)))

(define (any-event-send-event? event)
  (let ((f
	 (cond
	  ((key-event? event) key-event-send-event?)
	  ((button-event? event) button-event-send-event?)
	  ((motion-event? event) motion-event-send-event?)
	  ((crossing-event? event) crossing-event-send-event?)
	  ((focus-change-event? event) focus-change-event-send-event?)
	  ((expose-event? event) expose-event-send-event?)
	  ((graphics-expose-event? event) graphics-expose-event-send-event?)
	  ((no-expose-event? event) no-expose-event-send-event?)
	  ((visibility-event? event) visibility-event-send-event?)
	  ((create-window-event? event) create-window-event-send-event?)
	  ((destroy-window-event? event) destroy-window-event-send-event?)
	  ((unmap-event? event) unmap-event-send-event?)
	  ((map-event? event) map-event-send-event?)
	  ((map-request-event? event) map-request-event-send-event?)
	  ((reparent-event? event) reparent-event-send-event?)
	  ((configure-event? event) configure-event-send-event?)
	  ((gravity-event? event) gravity-event-send-event?)
	  ((resize-request-event? event) resize-request-event-send-event?)
	  ((configure-request-event? event)
	   configure-request-event-send-event?)
	  ((circulate-event? event) circulate-event-send-event?)
	  ((circulate-request-event? event)
	   circulate-request-event-send-event?)
	  ((property-event? event) property-event-send-event?)
	  ((selection-clear-event? event) selection-clear-event-send-event?)
	  ((selection-request-event? event) selection-request-event-send-event?)
	  ((selection-event? event) selection-event-send-event?)
	  ((colormap-event? event) colormap-event-send-event?)
	  ((client-message-event? event) client-message-event-send-event?)
	  ((mapping-event? event) mapping-event-send-event?)
	  ((keymap-event? event) keymap-event-send-event?))))
    (f event)))

(define (any-event-display event)
  (let ((f
	 (cond
	  ((key-event? event) key-event-display)
	  ((button-event? event) button-event-display)
	  ((motion-event? event) motion-event-display)
	  ((crossing-event? event) crossing-event-display)
	  ((focus-change-event? event) focus-change-event-display)
	  ((expose-event? event) expose-event-display)
	  ((graphics-expose-event? event) graphics-expose-event-display)
	  ((no-expose-event? event) no-expose-event-display)
	  ((visibility-event? event) visibility-event-display)
	  ((create-window-event? event) create-window-event-display)
	  ((destroy-window-event? event) destroy-window-event-display)
	  ((unmap-event? event) unmap-event-display)
	  ((map-event? event) map-event-display)
	  ((map-request-event? event) map-request-event-display)
	  ((reparent-event? event) reparent-event-display)
	  ((configure-event? event) configure-event-display)
	  ((gravity-event? event) gravity-event-display)
	  ((resize-request-event? event) resize-request-event-display)
	  ((configure-request-event? event) configure-request-event-display)
	  ((circulate-event? event) circulate-event-display)
	  ((circulate-request-event? event) circulate-request-event-display)
	  ((property-event? event) property-event-display)
	  ((selection-clear-event? event) selection-clear-event-display)
	  ((selection-request-event? event) selection-request-event-display)
	  ((selection-event? event) selection-event-display)
	  ((colormap-event? event) colormap-event-display)
	  ((client-message-event? event) client-message-event-display)
	  ((mapping-event? event) mapping-event-display)
	  ((keymap-event? event) keymap-event-display))))
    (f event)))

;; any-event-window does not return the window-element of some event,
;; but the first element that is a window - that is always the 5th
;; element. So it behaves like (XAnyEvent)e->window in C.

(define (any-event-window event)
  (let ((f
	 (cond
	  ((key-event? event) key-event-window)
	  ((button-event? event) button-event-window)
	  ((motion-event? event) motion-event-window)
	  ((crossing-event? event) crossing-event-window)
	  ((focus-change-event? event) focus-change-event-window)
	  ((expose-event? event) expose-event-window)
	  ((visibility-event? event) visibility-event-window)
	  ((create-window-event? event) create-window-event-parent)
	  ((destroy-window-event? event) destroy-window-event-event)
	  ((unmap-event? event) unmap-event-event)
	  ((map-event? event) map-event-event)
	  ((map-request-event? event) map-request-event-parent)
	  ((reparent-event? event) reparent-event-event)
	  ((configure-event? event) configure-event-event)
	  ((gravity-event? event) gravity-event-event)
	  ((resize-request-event? event) resize-request-event-window)
	  ((configure-request-event? event) configure-request-event-parent)
	  ((circulate-event? event) circulate-event-event)
	  ((circulate-request-event? event) circulate-request-event-parent)
	  ((property-event? event) property-event-window)
	  ((selection-clear-event? event) selection-clear-event-window)
	  ((selection-request-event? event) selection-request-event-owner)
	  ((selection-event? event) selection-event-requestor)
	  ((colormap-event? event) colormap-event-window)
	  ((client-message-event? event) client-message-event-window)
	  ((mapping-event? event) mapping-event-window)
	  ; exceptions:
	  ((no-expose-event? event) no-expose-event-drawable)
	  ((graphics-expose-event? event) graphics-expose-event-drawable)
          ; keymap-event
	  (else (lambda (x) #f)))))
    (f event)))

;; *******************************************************************

(define-record-type key-event :key-event
  (create-key-event type serial send-event? display window root subwindow 
			 time x y x-root y-root state keycode same-screen?)
  key-event?
  (type key-event-type)
  (serial key-event-serial)
  (send-event? key-event-send-event?)
  (display key-event-display)
  (window key-event-window)
  (root key-event-root)
  (subwindow key-event-subwindow)
  (time key-event-time)
  (x key-event-x)
  (y key-event-y)
  (x-root key-event-x-root)
  (y-root key-event-y-root)
  (state key-event-state)
  (keycode key-event-keycode)
  (same-screen? key-event-same-screen?))

(define-record-type button-event :button-event
  (create-button-event type serial send-event? display window root
			    subwindow time x y x-root y-root state button
			    same-screen?)
  button-event?
  (type button-event-type)
  (serial button-event-serial)
  (send-event? button-event-send-event?)
  (display button-event-display)
  (window button-event-window)
  (root button-event-root)
  (subwindow button-event-subwindow)
  (time button-event-time)
  (x button-event-x)
  (y button-event-y)
  (x-root button-event-x-root)
  (y-root button-event-y-root)
  (state button-event-state)
  (button button-event-button)
  (same-screen? button-event-same-screen?))

(define-record-type motion-event :motion-event
  (create-motion-event type serial send-event? display window root
			    subwindow time x y x-root y-root state is-hint?
			    same-screen?)
  motion-event?
  (type motion-event-type)
  (serial motion-event-serial)
  (send-event? motion-event-send-event?)
  (display motion-event-display)
  (window motion-event-window)
  (root motion-event-root)
  (subwindow motion-event-subwindow)
  (time motion-event-time)
  (x motion-event-x)
  (y motion-event-y)
  (x-root motion-event-x-root)
  (y-root motion-event-y-root)
  (state motion-event-state)
  (is-hint? motion-event-is-hint?)
  (same-screen? motion-event-same-screen?))

(define-record-type crossing-event :crossing-event
  (create-crossing-event type serial send-event? display window root 
			      subwindow time x y x-root y-root mode detail 
			      same-screen? focus? state)
  crossing-event?
  (type crossing-event-type)
  (serial crossing-event-serial)
  (send-event? crossing-event-send-event?)
  (display crossing-event-display)
  (window crossing-event-window)
  (root crossing-event-root)
  (subwindow crossing-event-subwindow)
  (time crossing-event-time)
  (x crossing-event-x)
  (y crossing-event-y)
  (x-root crossing-event-x-root)
  (y-root crossing-event-y-root)
  (mode crossing-event-mode)
  (detail crossing-event-detail)
  (same-screen? crossing-event-same-screen?)
  (focus? crossing-event-focus?)
  (state crossing-event-state))

(define-record-type focus-change-event :focus-change-event
  (create-focus-change-event type serial send-event? display window mode 
				  detail)
  focus-change-event?
  (type focus-change-event-type)
  (serial focus-change-event-serial)
  (send-event? focus-change-event-send-event?)
  (display focus-change-event-display)
  (window focus-change-event-window)
  (mode focus-change-event-mode)
  (detail focus-change-event-detail))

(define-record-type expose-event :expose-event
  (create-expose-event type serial send-event? display window x y width 
			    height count)
  expose-event?
  (type expose-event-type)
  (serial expose-event-serial)
  (send-event? expose-event-send-event?)
  (display expose-event-display)
  (window expose-event-window)
  (x expose-event-x)
  (y expose-event-y)
  (width expose-event-width)
  (height expose-event-height)
  (count expose-event-count))

(define-record-type graphics-expose-event :graphics-expose-event
  (create-graphics-expose-event type serial send-event? display drawable
				     x y width height count major-code
				     minor-code)
  graphics-expose-event?
  (type graphics-expose-event-type)
  (serial graphics-expose-event-serial)
  (send-event? graphics-expose-event-send-event?)
  (display graphics-expose-event-display)
  (drawable graphics-expose-event-drawable)
  (x graphics-expose-event-x)
  (y graphics-expose-event-y)
  (width graphics-expose-event-width)
  (height graphics-expose-event-height)
  (count graphics-expose-event-count)
  (major-code graphics-expose-event-major-code)
  (minor-code graphics-expose-event-minor-code))

(define-record-type no-expose-event :no-expose-event
  (create-no-expose-event type serial send-event? display drawable
			       major-code minor-code)
  no-expose-event?
  (type no-expose-event-type)
  (serial no-expose-event-serial)
  (send-event? no-expose-event-send-event?)
  (display no-expose-event-display)
  (drawable no-expose-event-drawable)
  (major-code no-expose-event-major-code)
  (minor-code no-expose-event-minor-code))

(define-record-type visibility-event :visibility-event
  (create-visibility-event type serial send-event? display window state)
  visibility-event?
  (type visibility-event-type)
  (serial visibility-event-serial)
  (send-event? visibility-event-send-event?)
  (display visibility-event-display)
  (window visibility-event-window)
  (state visibility-event-state))

(define-record-type create-window-event :create-window-event
  (create-create-window-event type serial send-event? display parent
				   window x y width height border-width 
				   override-redirect?)
  create-window-event?
  (type create-window-event-type)
  (serial create-window-event-serial)
  (send-event? create-window-event-send-event?)
  (display create-window-event-display)
  (parent create-window-event-parent)
  (window create-window-event-window)
  (x create-window-event-x)
  (y create-window-event-y)
  (width create-window-event-width)
  (height create-window-event-height)
  (border-width create-window-event-border-width)
  (override-redirect? create-window-event-override-redirect?))

(define-record-type destroy-window-event :destroy-window-event
  (create-destroy-window-event type serial send-event? display event
				    window)
  destroy-window-event?
  (type destroy-window-event-type)
  (serial destroy-window-event-serial)
  (send-event? destroy-window-event-send-event?)
  (display destroy-window-event-display)
  (event destroy-window-event-event)
  (window destroy-window-event-window))

(define-record-type unmap-event :unmap-event
  (create-unmap-event type serial send-event? display event window 
			   from-configure?)
  unmap-event?
  (type unmap-event-type)
  (serial unmap-event-serial)
  (send-event? unmap-event-send-event?)
  (display unmap-event-display)
  (event unmap-event-event)
  (window unmap-event-window)
  (from-configure? unmap-event-from-configure?))

(define-record-type map-event :map-event
  (create-map-event type serial send-event? display event window 
			 override-redirect?)
  map-event?
  (type map-event-type)
  (serial map-event-serial)
  (send-event? map-event-send-event?)
  (display map-event-display)
  (event map-event-event)
  (window map-event-window)
  (override-redirect? map-event-override-redirect?))

(define-record-type map-request-event :map-request-event
  (create-map-request-event type serial send-event? display parent window)
  map-request-event?
  (type map-request-event-type)
  (serial map-request-event-serial)
  (send-event? map-request-event-send-event?)
  (display map-request-event-display)
  (parent map-request-event-parent)
  (window map-request-event-window))

(define-record-type reparent-event :reparent-event
  (create-reparent-event type serial send-event? display event window
			      parent x y override-redirect?)
  reparent-event?
  (type reparent-event-type)
  (serial reparent-event-serial)
  (send-event? reparent-event-send-event?)
  (display reparent-event-display)
  (event reparent-event-event)
  (window reparent-event-window)
  (parent reparent-event-parent)
  (x reparent-event-x)
  (y reparent-event-y)
  (override-redirect? reparent-event-override-redirect?))

(define-record-type configure-event :configure-event
  (create-configure-event type serial send-event? display event window 
			       x y width height border-width above 
			       override-redirect?)
  configure-event?
  (type configure-event-type)
  (serial configure-event-serial)
  (send-event? configure-event-send-event?)
  (display configure-event-display)
  (event configure-event-event)
  (window configure-event-window)
  (x configure-event-x)
  (y configure-event-y)
  (width configure-event-width)
  (height configure-event-height)
  (border-width configure-event-border-width)
  (above configure-event-above)
  (override-redirect? configure-event-override-redirect?))

(define-record-type gravity-event :gravity-event
  (create-gravity-event type serial send-event? display event window x y)
  gravity-event?
  (type gravity-event-type)
  (serial gravity-event-serial)
  (send-event? gravity-event-send-event?)
  (display gravity-event-display)
  (event gravity-event-event)
  (window gravity-event-window)
  (x gravity-event-x)
  (y gravity-event-y))

(define-record-type resize-request-event :resize-request-event
  (create-resize-request-event type serial send-event? display window 
				    width height)
  resize-request-event?
  (type resize-request-event-type)
  (serial resize-request-event-serial)
  (send-event? resize-request-event-send-event?)
  (display resize-request-event-display)
  (window resize-request-event-window)
  (width resize-request-event-width)
  (height resize-request-event-height))

(define-record-type configure-request-event :configure-request-event
  (create-configure-request-event type serial send-event? display parent 
				       window window-change-alist)
  configure-request-event?
  (type configure-request-event-type)
  (serial configure-request-event-serial)
  (send-event? configure-request-event-send-event?)
  (display configure-request-event-display)
  (parent configure-request-event-parent)
  (window configure-request-event-window)
  (window-change-alist configure-request-event-window-change-alist))

(define-record-type circulate-event :circulate-event
  (create-circulate-event type serial send-event? display event window 
			       place)
  circulate-event?
  (type circulate-event-type)
  (serial circulate-event-serial)
  (send-event? circulate-event-send-event?)
  (display circulate-event-display)
  (event circulate-event-event)
  (window circulate-event-window)
  (place circulate-event-place))

(define-record-type circulate-request-event :circulate-request-event
  (create-circulate-request-event type serial send-event? display parent 
				       window place)
  circulate-request-event?
  (type circulate-request-event-type)
  (serial circulate-request-event-serial)
  (send-event? circulate-request-event-send-event?)
  (display circulate-request-event-display)
  (parent circulate-request-event-parent)
  (window circulate-request-event-window)
  (place circulate-request-event-place))

(define-record-type property-event :property-event
  (create-property-event type serial send-event? display window atom time
			      state)
  property-event?
  (type property-event-type)
  (serial property-event-serial)
  (send-event? property-event-send-event?)
  (display property-event-display)
  (window property-event-window)
  (atom property-event-atom)
  (time property-event-time)
  (state property-event-state))

(define-record-type selection-clear-event :selection-clear-event
  (create-selection-clear-event type serial send-event? display window 
				     selection time)
  selection-clear-event?
  (type selection-clear-event-type)
  (serial selection-clear-event-serial)
  (send-event? selection-clear-event-send-event?)
  (display selection-clear-event-display)
  (window selection-clear-event-window)
  (selection selection-clear-event-selection)
  (time selection-clear-event-time))

(define-record-type selection-request-event :selection-request-event
  (create-selection-request-event type serial send-event? display owner 
				       requestor selection target property
				       time)
  selection-request-event?
  (type selection-request-event-type)
  (serial selection-request-event-serial)
  (send-event? selection-request-event-send-event?)
  (display selection-request-event-display)
  (owner selection-request-event-owner)
  (requestor selection-request-event-requestor)
  (selection selection-request-event-selection)
  (target selection-request-event-target)
  (property selection-request-event-property)
  (time selection-request-event-time))

(define-record-type selection-event :selection-event
  (create-selection-event type serial send-event? display requestor
			       selection target property time)
  selection-event?
  (type selection-event-type)
  (serial selection-event-serial)
  (send-event? selection-event-send-event?)
  (display selection-event-display)
  (requestor selection-event-requestor)
  (selection selection-event-selection)
  (target selection-event-target)
  (property selection-event-property)
  (time selection-event-time))

(define-record-type colormap-event :colormap-event
  (create-colormap-event type serial send-event? display window colormap
			      new? state)
  colormap-event?
  (type colormap-event-type)
  (serial colormap-event-serial)
  (send-event? colormap-event-send-event?)
  (display colormap-event-display)
  (window colormap-event-window)
  (colormap colormap-event-colormap)
  (new? colormap-event-new?)
  (state colormap-event-state))

(define-record-type client-message-event :client-message-event
  (create-client-message-event type serial send-event? display window
			       property)
  client-message-event?
  (type client-message-event-type)
  (serial client-message-event-serial)
  (send-event? client-message-event-send-event?)
  (display client-message-event-display)
  (window client-message-event-window)
  (property client-message-event-property))

(define-record-type mapping-event :mapping-event
  (create-mapping-event type serial send-event? display window request 
			     first-keycode count)
  mapping-event?
  (type mapping-event-type)
  (serial mapping-event-serial)
  (send-event? mapping-event-send-event?)
  (display mapping-event-display)
  (window mapping-event-window)
  (request mapping-event-request)
  (first-keycode mapping-event-first-keycode)
  (count mapping-event-count))

(define-record-type keymap-event :keymap-event
  (create-keymap-event type serial send-event? display bit-vector)
  keymap-event?
  (type keymap-event-type)
  (serial keymap-event-serial)
  (send-event? keymap-event-send-event?)
  (display keymap-event-display)
  (bit-vector keymap-event-bit-vector))

(define-exported-binding "scx-key-event" :key-event)
(define-exported-binding "scx-button-event" :button-event)
(define-exported-binding "scx-motion-event" :motion-event)
(define-exported-binding "scx-crossing-event" :crossing-event)
(define-exported-binding "scx-focus-change-event" :focus-change-event)
(define-exported-binding "scx-expose-event" :expose-event)
(define-exported-binding "scx-graphics-expose-event" :graphics-expose-event)
(define-exported-binding "scx-no-expose-event" :no-expose-event)
(define-exported-binding "scx-visibility-event" :visibility-event)
(define-exported-binding "scx-create-window-event" :create-window-event)
(define-exported-binding "scx-destroy-window-event" :destroy-window-event)
(define-exported-binding "scx-unmap-event" :unmap-event)
(define-exported-binding "scx-map-event" :map-event)
(define-exported-binding "scx-map-request-event" :map-request-event)
(define-exported-binding "scx-reparent-event" :reparent-event)
(define-exported-binding "scx-configure-event" :configure-event)
(define-exported-binding "scx-gravity-event" :gravity-event)
(define-exported-binding "scx-resize-request-event" :resize-request-event)
(define-exported-binding "scx-configure-request-event"
  :configure-request-event)
(define-exported-binding "scx-circulate-event" :circulate-event)
(define-exported-binding "scx-circulate-request-event"
  :circulate-request-event)
(define-exported-binding "scx-property-event" :property-event)
(define-exported-binding "scx-selection-clear-event" :selection-clear-event)
(define-exported-binding "scx-selection-request-event"
  :selection-request-event)
(define-exported-binding "scx-selection-event" :selection-event)
(define-exported-binding "scx-colormap-event" :colormap-event)
(define-exported-binding "scx-client-message-event" :client-message-event)
(define-exported-binding "scx-mapping-event" :mapping-event)
(define-exported-binding "scx-keymap-event" :keymap-event)
