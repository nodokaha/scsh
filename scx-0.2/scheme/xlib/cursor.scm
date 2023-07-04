;; Copyright (c) 2001-2003 by David Frese

;; *** create cursors ************************************************

(import-xlib-function create-pixmap-cursor
  (display source mask foreground-color background-color x y)
  "scx_Create_Pixmap_Cursor")

;; source-char and mask-char have to be integers.
(import-xlib-function create-glyph-cursor
  (display source-font mask-font source-char mask-char foreground-color
	   background-color)
  "scx_Create_Glyph_Cursor")

(import-xlib-function create-font-cursor (display shape)
  "scx_Create_Font_Cursor")

(define xc-X-cursor 0)
(define xc-arrow 2)
(define xc-based-arrow-down 4)
(define xc-based-arrow-up 6)
(define xc-boat 8)
(define xc-bogosity 10)
(define xc-bottom-left-corner 12)
(define xc-bottom-right-corner 14)
(define xc-bottom-side 16)
(define xc-bottom-tee 18)
(define xc-box-spiral 20)
(define xc-center-ptr 22)
(define xc-circle 24)
(define xc-clock 26)
(define xc-coffee-mug 28)
(define xc-cross 30)
(define xc-cross-reverse 32)
(define xc-crosshair 34)
(define xc-diamond-cross 36)
(define xc-dot 38)
(define xc-dotbox 40)
(define xc-double-arrow 42)
(define xc-draft-large 44)
(define xc-draft-small 46)
(define xc-draped-box 48)
(define xc-exchange 50)
(define xc-fleur 52)
(define xc-gobbler 54)
(define xc-gumby 56)
(define xc-hand1 58)
(define xc-hand2 60)
(define xc-heart 62)
(define xc-icon 64)
(define xc-iron-cross 66)
(define xc-left-ptr 68)
(define xc-left-side 70)
(define xc-left-tee 72)
(define xc-leftbutton 74)
(define xc-ll-angle 76)
(define xc-lr-angle 78)
(define xc-man 80)
(define xc-middlebutton 82)
(define xc-mouse 84)
(define xc-pencil 86)
(define xc-pirate 88)
(define xc-plus 90)
(define xc-question-arrow 92)
(define xc-right-ptr 94)
(define xc-right-side 96)
(define xc-right-tee 98)
(define xc-rightbutton 100)
(define xc-rtl-logo 102)
(define xc-sailboat 104)
(define xc-sb-down-arrow 106)
(define xc-sb-h-double-arrow 108)
(define xc-sb-left-arrow 110)
(define xc-sb-right-arrow 112)
(define xc-sb-up-arrow 114)
(define xc-sb-v-double-arrow 116)
(define xc-shuttle 118)
(define xc-sizing 120)
(define xc-spider 122)
(define xc-spraycan 124)
(define xc-star 126)
(define xc-target 128)
(define xc-tcross 130)
(define xc-top-left-arrow 132)
(define xc-top-left-corner 134)
(define xc-top-right-corner 136)
(define xc-top-side 138)
(define xc-top-tee 140)
(define xc-trek 142)
(define xc-ul-angle 144)
(define xc-umbrella 146)
(define xc-ur-angle 148)
(define xc-watch 150)
(define xc-xterm 152)

;; *** define cursors ************************************************

(import-xlib-function define-cursor (display window cursor)
  "scx_Define_Cursor")

(import-xlib-function undefine-cursor (display window)
  "scx_Undefine_Cursor")

;; *** manipulate cursors ********************************************

(import-xlib-function recolor-cursor
  (display cursor foreground-color background-color)
  "scx_Recolor_Cursor")

(import-xlib-function free-cursor (display cursor)
  "scx_Free_Cursor")

;; query-best-cursor defined in gcontext.scm
