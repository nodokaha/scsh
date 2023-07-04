;; Copyright (c) 2001-2003 by David Frese

;; a keysym is a 16-bit protocol ID (see X11/keysymdef.h) 

;; a keycode is an integer specifying a single key on the keyboard
;; (hardware depended)

;; *** manipulate keyboard encoding **********************************

;; a keyboard mapping is a list of lists of keysyms

(import-xlib-function change-keyboard-mapping
  (display first-keycode keysyms-lists)
  "scx_Change_Keyboard_Mapping")

;; returns keycode-count lists of keysyms
(import-xlib-function get-keyboard-mapping
  (display first-keycode keycode-count)
  "scx_Get_Keyboard_Mapping")

;; returns a pair (min-keycodes . max-keycodes)
(import-xlib-function display-keycodes (display)
  "scx_Display_Keycodes")

;; a modmap is an alist mapping a modifier to a list of
;; keycodes. Valid modifiers are (state shift) (state lock) (state
;; control) (state mod1) (state mod2) (state mod3) (state mod4)
;; (state mod5)

(import-xlib-function set-modifier-mapping (display modmap)
  "scx_Set_Modifier_Mapping")

(import-xlib-function get-modifier-mapping (display)
  "scx_Get_Modifier_Mapping")

;; *** convert keysyms ***********************************************

(import-lambda-definition string->keysym (string)
  "scx_String_To_Keysym")

(import-lambda-definition keysym->string (keysym)
  "scx_Keysym_To_String")

;; TODO include X11/keysymdef.h ??

(import-xlib-function keycode->keysym (display keycode index)
  "scx_Keycode_To_Keysym")

(import-xlib-function keysym->keycode (display keysym)
  "scx_Keysym_To_Keycode")

;; returns a pair (lower . upper)
(import-lambda-definition convert-case (keysym)
  "scx_Convert_Case")

(define (convert-to-lowercase keysym)
  (car (convert-case keysym)))

(define (convert-to-uppercase keysym)
  (cdr (convert-case keysym)))

;; *** handle keyboard input events in Latin-1 ***********************

(import-lambda-definition %lookup-keysym (key-event index)
  "scx_Lookup_Keysym")

(define (lookup-keysym key-event index)
  (call-xlib-function (key-event-display key-event) 'lookup-keysym
		      (lambda () (%lookup-keysym key-event index))))

(import-lambda-definition %refresh-keyboard-mapping (mapping-event)
  "scx_Refresh_Keyboard_Mapping")

(define (refresh-keyboard-mapping mapping-event)
  (call-xlib-function (mapping-event-display mapping-event)
		      'refresh-keyboard-mapping
		      (lambda () (%refresh-keyboard-mapping mapping-event))))

;; returns a pair (keysym . string)
(import-lambda-definition %lookup-string/keysym (key-event)
  "scx_Lookup_String")

(define (lookup-string/keysym key-event)
  (call-xlib-function (key-event-display key-event)
		      'lookup-string/keysym
		      (lambda () (%lookup-string/keysym key-event))))

(define (lookup-string key-event)
  (cdr (lookup-string/keysym key-event)))

(import-xlib-function rebind-keysym (display keysym mod-keysyms string)
  "scx_Rebind_Keysym")
