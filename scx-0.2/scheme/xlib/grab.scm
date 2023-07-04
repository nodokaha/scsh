;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

;; *** grab the pointer **********************************************

(define-enumerated-type grab-mode :grab-mode
  grab-mode? grab-modes grab-mode-name grab-mode-index
  (sync async))

(define-exported-binding "scx-grab-mode" :grab-mode)

(define-enumerated-type grab-status :grab-status
  grab-status? grab-states grab-status-name grab-status-index
  (success already-grabbed invalid-time not-viewable frozen))

(define-exported-binding "scx-grab-states" grab-states)

(import-xlib-function grab-pointer
  (display grab-window owner-events? events ptr-mode kbd-mode
	   confine-to cursor time)
  "scx_Grab_Pointer")

(import-xlib-function ungrab-pointer (display time)
  "scx_Ungrab_Pointer")

(import-xlib-function change-active-pointer-grab
  (display events cursor time)
  "scx_Change_Active_Pointer_Grab")

;; *** grab pointer buttons ******************************************

(define-enumerated-type state :state
  state? states state-name state-index
  (shift lock control mod1 mod2 mod3 mod4 mod5
   button1 button2 button3 button4 button5
   state-13 state-14
   any-modifier))

(define-exported-binding "scx-state" :state)
(define-exported-binding "scx-states" states)

(define-enum-set-type state-set :state-set
  state-set? make-state-set
  state state? states state-index)

(define-exported-binding "scx-state-set" :state-set)

(define-enumerated-type button :button
  button? buttons button-name button-index
  (any-button button1 button2 button3 button4 button5))

(define-exported-binding "scx-button" :button)
(define-exported-binding "scx-buttons" buttons)

(import-xlib-function grab-button
  (display button modifiers grab-window owner-events? events ptr-mode
	   kbd-mode confine-to cursor)
  "scx_Grab_Button")

(import-xlib-function ungrab-button (display button modifiers grab-window)
  "scx_Ungrab_Button")

;; *** grab the keyboard *********************************************

(import-xlib-function grab-keyboard
  (display grab-window owner-events? ptr-mode kbd-mode time)
  "scx_Grab_Keyboard")

(import-xlib-function ungrab-keyboard (display time)
  "scx_Ungrab_Keyboard")

;; *** grab keyboard keys ********************************************

(import-xlib-function grab-key
  (display keycode modifiers grab-window owner-events? ptr-mode kbd-mode)
  "scx_Grab_Key")

(import-xlib-function ungrab-key (display keycode modifiers grab-window)
  "scx_Ungrab_Key")

;; *** release queued events *****************************************

(define-enumerated-type event-mode :event-mode
  event-mode? event-modes event-mode-name event-mode-index
  (async-pointer sync-pointer replay-pointer async-keyboard
   sync-keyboard replay-keyboard async-both sync-both))

(define-exported-binding "scx-event-mode" :event-mode)

(import-xlib-function allow-events (display event-mode time)
  "scx_Allow_Events")

;; *** grab the server ***********************************************

;; grab-server disables processing of requests and close downs on all
;; other connections than the one this request arrived on. You should
;; not grab the X server any more than is absolutely necessary. See
;; XGrabServer.

(import-xlib-function grab-server (display)
  "scx_Grab_Server")

;; ungrab-server restarts processing of requests and close downs on
;; other connections.  You should avoid grabbing the X server as much
;; as possible. See XUngrabServer.

(import-xlib-function ungrab-server (display)
  "scx_Ungrab_Server")
