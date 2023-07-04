;; Copyright (c) 2001-2003 by David Frese, Norbert Freudemann

;; *** draw image text ***********************************************

(import-xlib-function draw-image-string (display drawable gc x y string)
  "scx_Draw_Image_String")

;; string has to be a list of (byte1 . byte2) pairs, where byte1 and
;; byte2 are characters
(import-xlib-function draw-image-string-16
  (display drawable gc x y string)
  "scx_Draw_Image_String_16")

;; *** draw polytext text ********************************************

(define-record-type text-item :text-item
  (make-text-item string delta font)
  text-item?
  (string text-item:string)
  (delta text-item:delta)
  (font text-item:font))

(define-exported-binding "scx-text-item" :text-item)

(define-syntax make-text-items
  (syntax-rules
   (change-font with-delta)
   ((make-text-items (change-font font) rest ...)
    (cons (make-text-item #f 0 font)
	  (make-text-items rest ...)))
   ((make-text-items (with-delta d text) rest ...)
    (cons (make-text-item text d none)
	  (make-text-items rest ...)))
   ((make-text-items text rest ...)
    (cons (make-text-item text 0 none)
	  (make-text-items rest ...)))
   ((make-text-items)
    '())))

(import-xlib-function draw-text (display drawable gc x y items)
  "scx_Draw_Text")

(import-xlib-function draw-text-16 (display drawable gc x y items)
  "scx_Draw_Text_16")

;; *** compute or query text extents *********************************

;; returns a char-struct record (the direction, font-ascent and
;; font-descent can be obtained from the font-struct directly)
(import-lambda-definition text-extents (font-struct string)
  "scx_Text_Extents")

;; string has to be a list of (byte1 . byte2) pairs, where byte1 and
;; byte2 are characters
(import-lambda-definition text-extents-16 (font-struct string)
  "scx_Text_Extents_16")
