;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

;; *** reparent windows **********************************************

;; If the specified window is mapped, reparent-window automatically
;; performs an UnmapWindow request on it, removes it from its current
;; position in the hierarchy, and inserts it as the child of the
;; specified parent. See XReparentWindow.

(import-xlib-function reparent-window (display window parent x y)
  "scx_Reparent_Window")

;; *** control colormaps *********************************************

;; install-colormap function installs the specified colormap for
;; its associated screen. See XInstallColormap.

(import-xlib-function install-colormap (display colormap)
  "scx_Install_Colormap")

;; uninstall-colormap removes the specified colormap from the required
;; list for its screen. See XUninstallColormap.

(import-xlib-function uninstall-colormap (display colormap)
  "scx_Uninstall_Colormap")

;; list-installed-colormaps function returns a list of the currently
;; installed colormaps for the screen of the specified window. See
;; XListInstalledColormaps.

(import-xlib-function list-installed-colormaps (display window)
  "scx_List_Installed_Colormaps")

;; *** control input focus *******************************************

;; set-input-focus function changes the input focus and the
;; last-focus-change time. See XSetInputFocus.

(import-xlib-function set-input-focus (display window revert-to time)
  "scx_Set_Input_Focus")

(define-enumerated-type revert-to :revert-to
  revert-to? revert-tos revert-to-name revert-to-index
  (none pointer-root parent))

(define-exported-binding "scx-revert-to" :revert-to)
(define-exported-binding "scx-revert-tos" revert-tos)

;; get-input-focus returns the current focus window and the current focus
;; state (revert-to) as a pair. See XGetInputFocus.

(import-xlib-function get-input-focus (display)
  "scx_Get_Input_Focus")

(define (get-input-focus-window display)
  (car (get-input-focus display)))

;; *** move pointer **************************************************

(import-xlib-function general-warp-pointer 
  (display src dest src-x src-y src-width src-height
	   dest-x dest-y)
  "scx_Warp_Pointer")

;; warp-pointer calls general-warp-pointer with using None as the
;; source window, and therefor moving the pointer to the destination
;; window unconditionally

(define (warp-pointer display dst-window dst-x dst-y)
  (general-warp-pointer display none dst-window
			0 0 0 0 dst-x dst-y))

;; warp-pointer-relative uses general-warp-pointer to move the pointer
;; by x-offset and y-offset away from it's current position.

(define (warp-pointer-relative display x-offset y-offset)
  (general-warp-pointer display none
			x-offset y-offset
			none
			0 0 0 0))

;; *** manipulate keyboard settings **********************************

;; XChangeKeyboardControl ?? TODO

;; bell rings the bell on the keyboard on the specified display, if
;; possible. The optional percent argument specifies the volume in a
;; range from -100 to 100. 0 is the default value. See XBell.

(import-xlib-function %bell (display percent)
  "scx_Bell")

(define (bell display . percent)
  (%bell display
	 (if (null? percent)
	     0
	     (car percent))))

;; *** control host access *******************************************

;; XAddHost etc. ?? TODO

;; set-access-control either enables or disables the use of the access
;; control list at each connection setup. See XSetAccessControl.

(import-xlib-function set-access-control (display enable?)
  "scx_Set_Access_Control")

;; *** change a client's save set ************************************

;; Depending on the specified mode, change-save-set either inserts or
;; deletes the specified window from the client's save-set. The
;; specified window must have been created by some other client, or a
;; BadMatch error results. mode is one of 'insert or 'delete. See
;; XChangeSaveSet.

(import-xlib-function change-save-set (display window mode)
  "scx_Change_Save_Set")

(define-enumerated-type save-set :save-set
  save-set? save-sets save-set-name save-set-index
  (insert delete))

(define-exported-binding "scx-save-set" :save-set)

;; *** control clients ***********************************************

;; set-close-down-mode defines what will happen to the client's
;; resources at connection close. mode is one of 'destroy-all,
;; 'retain-permanent or 'retain-temporary. See XSetCloseDownMode.

(define-enumerated-type close-down-mode :close-down-mode
  close-down-mode? close-down-modes close-down-mode-name close-down-mode-index
  (destroy-all retain-permanent retain-temporary))

(define-exported-binding "scx-close-down-mode" :close-down-mode)

(import-xlib-function set-close-down-mode (display mode)
  "scx_Set_Close_Down_Mode")

(import-xlib-function kill-client (display xid)
  "scx_Kill_Client")

;; *** manipulate pointer settings ***********************************

;; get-pointer-mapping returns a vector, that specifies in the i-th
;; element the logical button number for the physical button i+1. See
;; XGetPointerMapping.

(import-xlib-function get-pointer-mapping (display)
  "scx_Get_Pointer_Mapping")

;; set-pointer-mapping sets the mapping of the pointer. mapping must
;; be a vector of the same length that get-pointer-mapping would
;; return. If any of the buttons to be altered are logically in the
;; down state, then #f is returned and the mapping is not changed, #t
;; otherwise. See XSetPointerMapping.

(import-xlib-function set-pointer-mapping (display mapping)
  "scx_Set_Pointer_Mapping")

;; TODO: there is a lot more...
;; WM_STATE property

;(define (get-wm-state window)
;  (let* ((dpy (window-display window))
;	 (a (intern-atom dpy "WM_STATE"))
;	 (v.t.f (get-property window a #f)))
;    (if (and v.t.f
;	     (eq? (cadr v.t.f) a)
;	     (>= (vector-length (car v.t.f)) 2))
;	(let ((v (car v.t.f)))
;	  (list (integer->wm-state (vector-ref v 0))
;		(make-window (vector-ref v 1) dpy #f)))
;	#f)))

;(define (set-wm-state window wm-state icon-window)
;  (let* ((dpy (window-display window))
;	 (a (intern-atom dpy "WM_STATE")))
;    (change-property window a a 32
;		     (list->vector (list (wm-state->integer wm-state)
;					 (window-Xwindow icon-window))))))

;(define-enumerated-type wm-state :wm-state
;  wm-state? wm-states wm-state-name wm-state-index
;  (withdrawn normal wm-state-2 iconic))

;(define (integer->wm-state i)
;  (vector-ref wm-states i))

;(define (wm-state->integer s)
;  (wm-state-index s))
