;; *** manipulate toplevel windows ***********************************

;; iconfiy-window send a WM_CHANGE_STATE message (in an appropiate
;; format), to the root window of the specified screen. See
;; XIconifyWindow.

;; returns #f on error.
(import-xlib-function iconify-window (display window screen-num)
  "scx_Iconify_Window")

;; withdraw-window unmaps the specified window and sends a synthetic
;; UnmapNotify event to the root window of the specified screen. See
;; XWithdrawWindow.

;; returns #f on error.
(import-xlib-function withdraw-window (display window scr-num)
  "scx_Withdraw_Window")

;; reconfigure-wm-window changes attributes of the specified window
;; similar to configure-window, or sends a ConfigureRequestEvent to
;; the root window if that fails. See XReconfigureWMWindow. See
;; configure-window.

;; returns #f on error.
(import-xlib-function reconfigure-wm-window 
			  (display window scr-num changes)
  "scx_Reconfigure_Wm_Window")

;; *** set or read a window's WM_* properties ************************

;; get-wm-command reads the WM_COMMAND property from the specified
;; window and returns it as a list of strings. See XGetCommand.

;; returns #f on error.
(import-xlib-function get-wm-command (display window)
  "scx_Get_Wm_Command")

;; set-wm-command! sets the WM_COMMAND property (the command and
;; arguments used to invoke the application). The command has to be
;; specified as a list of strings. See XSetCommand.

(import-xlib-function set-wm-command! (display window command)
  "scx_Set_Wm_Command")

;; get-wm-protocols function returns the list of atoms stored in the
;; WM_PROTOCOLS property on the specified window, or #f if this
;; property does not exist or has a bas format. These atoms describe
;; window manager protocols in which the owner of this window is
;; willing to participate. See XGetWMProtocols.

;; returns #f on error.
(import-xlib-function get-wm-protocols (display window)
  "scx_Get_Wm_Protocols")

;; set-wm-protocols! sets the WM_PROTOCOLS property of the specified
;; window. protocols has to be a list of atoms. See XSetWMProtocols.

;; returns #f on error.
(import-xlib-function set-wm-protocols! (display window protocols)
  "scx_Set_Wm_Protocols")

;; get-wm-class returns the class hint for the specified window. That
;; is a pair of strings (name . class) See XGetClassHint.

;; returns #f on error.
(import-xlib-function get-wm-class (display window)
  "scx_Get_Wm_Class")

;; set-wm-class! sets the class hint for the specified window. See
;; XSetClassHint.

;; returns #f on error.
(import-xlib-function set-wm-class! (display window name class)
  "scx_Set_Wm_Class")

;; *** set or read a window's WM_HINTS property **********************

(define-enumerated-type initial-state :initial-state
  initial-state? initial-states initial-state-name initial-state-index
  (withdrawn normal initial-state-2 iconic initial-state-4))

(define-exported-binding "scx-initial-state" :initial-state)
(define-exported-binding "scx-initial-states" initial-states)

(define-enumerated-type wm-hint :wm-hint
  wm-hint? wm-hints wm-hint-name wm-hint-index
  (input? initial-state icon-pixmap icon-window icon-position icon-mask
   window-group wm-hint-7 urgency))

(define-exported-binding "scx-wm-hint" :wm-hint)
(define-exported-binding "scx-wm-hints" wm-hints)

(define-syntax make-wm-hint-alist
  (syntax-rules 
   ()
   ((make-wm-hint-alist (attr arg) rest ...)
    (cons (cons (wm-hint attr) arg)
	  (make-wm-hint-alist rest ...)))
   ((make-wm-hint-alist)
    '())))

;; get-wm-hints reads the window manager hints and returns them as an
;; alist mapping wm-hint types to specific values. See wm-hint.  See
;; XGetWMHints for a description.

(import-xlib-function get-wm-hints (display window)
  "scx_Get_Wm_Hints")

;; set-wm-hints! sets the specified window manager hints. The hints
;; must be specified as an alist of wm-hint values (see above) mapping
;; to the appropiate values. See XSetWMHints.

(import-xlib-function set-wm-hints! (display window wm-hint-alist)
  "scx_Set_Wm_Hints")

;; get-transient-for returns the WM_TRANSIENT_FOR property for the
;; specified window. The value of that property is a window. See
;; XGetTransientForHint.

(import-xlib-function get-transient-for (display window)
  "scx_Get_Transient_For")

;; set-transient-for! sets the WM_TRANSIENT_FOR property of the
;; specified window to the specified property-window. See
;; XSetTransientForHint.

(import-xlib-function set-transient-for! (display window prop_window)
  "scx_Set_Transient_For")

;; get-text-property returns the property specified by atom of the
;; specified window as a property record. See get-window-property. See
;; XGetTextProperty.

(import-xlib-function get-text-property (display window atom)
  "scx_Get_Text_Property")

;; set-text-property! sets the property specified by atom of the
;; specified window to value - a property record.

(import-xlib-function set-text-property! (display window value atom)
  "scx_Set_Text_Property")

(define (property->string-list property)
  (string->string-list (property:data property)))

(define xa-string 31) ;; defined in Xatom.h

(define (string-list->property strings)
  (make-property xa-string (property-format char)
		 (string-list->string strings)))

;; The following function a wrappers for the get/set-text-property
;; function.

(define xa-wm-name 39)
(define xa-wm-icon-name 37)
(define xa-wm-client-machine 36)

(define (get-wm-name display w)
  (get-text-property display w xa-wm-name))

(define (get-wm-icon-name display w)
  (get-text-property display w xa-wm-icon-name))

(define (get-wm-client-machine display w)
  (get-text-property display w xa-wm-client-machine))

(define (set-wm-name! display w s)
  (set-text-property! display w s xa-wm-name))

(define (set-wm-icon-name! display w s)
  (set-text-property! display w s xa-wm-icon-name))

(define (set-wm-client-machine! display w s)
  (set-text-property! display w s xa-wm-client-machine))

;; an enumerated type for XSizeHints used by get-wm-normal-hints and
;; set-wm-normal-hints!

(define-enumerated-type size-hint :size-hint
  size-hint?
  size-hints
  size-hint-name
  size-hint-index
  ;; aspect should have the form ((min-x . min-y) . (max-x . max-y))
  ;; for win-gravity see gravity in create-window.
  ;; the other hints must be pairs of integers - (x . y) or (width . height)
  ;; us-position, us-size .....!!??
  (us-position us-size position size min-size max-size resize-inc aspect
   base-size win-gravity))

(define-exported-binding "scx-size-hint" :size-hint)
(define-exported-binding "scx-size-hints" size-hints)

(define-syntax make-size-hint-alist
  (syntax-rules 
   ()
   ((make-size-hint-alist (attr arg) rest ...)
    (cons (cons (size-hint attr) arg)
	  (make-size-hint-alist rest ...)))
   ((make-size-hint-alist)
    '())))

;; get-wm-normal-hints/set-wm-normal-hints! get or set the size hints
;; stored in the WM_NORMAL_HINTS property on the specified window. The
;; hints are '(x y width height us-position us-size min-width
;; min-height max-width max-height width-inc height-inc min-aspect-x
;; min-aspect-y max-aspect-x max-aspect-y base-width base-height
;; gravity). See XGetWMNormalHints, XSetWMNormalHints.

(import-xlib-function get-wm-normal-hints (display window)
  "scx_Get_Wm_Normal_Hints")

(import-xlib-function set-wm-normal-hints! (display window alist)
  "scx_Set_Wm_Normal_Hints")

;; get-icon-sizes returns the icon sizes specified by a window manager
;; as a list. See XGetIconSizes.

(define-record-type icon-size :icon-size
  (make-icon-size min-width min-height max-width max-height width-inc
		  height-inc)
  icon-size?
  (min-width icon-size:min-width set-icon-size:min-width!)
  (min-height icon-size:min-height set-icon-size:min-height!)
  (max-width icon-size:max-width set-icon-size:max-width!)
  (max-height icon-size:max-height set-icon-size:max-height!)
  (width-inc icon-size:width-inc set-icon-size:width-inc!)
  (height-inc icon-size:height-inc set-icon-size:height-inc!))

(define-exported-binding "scx-icon-size" :icon-size)

(import-xlib-function get-icon-sizes (display window)
  "scx_Get_Icon_Sizes")

;; set-icon-sizes! is used only by window managers to set the
;; supported icon sizes. See icon-sizes, XSetIconSizes.

(import-xlib-function set-icon-sizes! (display window sizes)
  "scx_Set_Icon_Sizes")
