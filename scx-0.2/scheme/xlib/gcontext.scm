;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

;; GC is a pointer to a C structure
;; GContext is the protocol ID

;; *** GC type *******************************************************

(define-record-type gc :gc
  (make-gc cpointer)
  gc?
  (cpointer gc-cpointer))

(define-exported-binding "scx-gc" :gc)

;; *** GC values and types *******************************************

(define-enumerated-type gc-function :gc-function
  gc-function? gc-functions gc-function-name gc-function-index
  (clear and and-reverse copy and-inverted no-op xor or nor equiv
   invert or-reverse copy-inverted or-inverted nand set))

(define-exported-binding "scx-gc-function" :gc-function)
(define-exported-binding "scx-gc-functions" gc-functions)

(define-enumerated-type line-style :line-style
  line-style? line-styles line-style-name line-style-index
  (solid on-off-dash double-dash))

(define-exported-binding "scx-line-style" :line-style)
(define-exported-binding "scx-line-styles" line-styles)

(define-enumerated-type cap-style :cap-style
  cap-style? cap-styles cap-style-name cap-style-index
  (not-last butt round projecting))

(define-exported-binding "scx-cap-style" :cap-style)
(define-exported-binding "scx-cap-styles" cap-styles)

(define-enumerated-type join-style :join-style
  join-style? join-styles join-style-name join-style-index
  (miter round bevel))

(define-exported-binding "scx-join-style" :join-style)
(define-exported-binding "scx-join-styles" join-styles)

(define-enumerated-type fill-style :fill-style
  fill-style? fill-styles fill-style-name fill-style-index
  (solid tiled stippled opaque-stippled))

(define-exported-binding "scx-fill-style" :fill-style)
(define-exported-binding "scx-fill-styles" fill-styles)

(define-enumerated-type fill-rule :fill-rule
  fill-rule? fill-rules fill-rule-name fill-rule-index
  (even-odd winding))

(define-exported-binding "scx-fill-rule" :fill-rule)
(define-exported-binding "scx-fill-rules" fill-rules)

(define-enumerated-type subwindow-mode :subwindow-mode
  subwindow-mode? subwindow-modes subwindow-mode-name subwindow-mode-index
  (clip-by-children include-inferiors))

(define-exported-binding "scx-subwindow-mode" :subwindow-mode)
(define-exported-binding "scx-subwindow-modes" subwindow-modes)

(define-enumerated-type arc-mode :arc-mode
  arc-mode? arc-modes arc-mode-name arc-mode-index
  (chord pie-slice))

(define-exported-binding "scx-arc-mode" :arc-mode)
(define-exported-binding "scx-arc-modes" arc-modes)

(define-enumerated-type gc-value :gc-value
  gc-value?
  gc-values
  gc-value-name
  gc-value-index
  (function plane-mask foreground background line-width line-style cap-style
   join-style fill-style fill-rule tile stipple ts-x-origin ts-y-origin
   font subwindow-mode graphics-exposures clip-x-origin clip-y-origin
   clip-mask dash-offset dashes arc-mode))

(define all-gc-values (vector->list gc-values))

(define-exported-binding "scx-gc-value" :gc-value)
(define-exported-binding "scx-gc-values" gc-values)

(define-syntax make-gc-value-alist
  (syntax-rules 
   ()
   ((make-gc-value-alist (attr arg) rest ...)
    (cons (cons (gc-value attr) arg)
	  (make-gc-value-alist rest ...)))
   ((make-gc-value-alist)
    '())))

(define-enum-set-type gc-value-set :gc-value-set
  gc-value-set? make-gc-value-set
  gc-value gc-value? gc-values gc-value-index)

(define-exported-binding "scx-gc-value-set" :gc-value-set)

;; *** create or free graphics contexts ******************************

(import-xlib-function create-gc (display drawable gc-value-alist)
  "scx_Create_Gc")

(import-xlib-function copy-gc! (display srck dest mask)
  "scx_Copy_Gc")

(define (copy-gc display drawable src)
  (let ((gc (create-gc display drawable '())))
    (copy-gc! display src all-gc-values gc)
    gc))

(import-xlib-function change-gc (display gc values)
  "scx_Change_Gc")

(define (make-gc-setter name)
  (lambda (display gc value)
    (change-gc display gc (list (cons name value)))))

(define set-gc-function! (make-gc-setter (gc-value function)))
(define set-gc-plane-mask! (make-gc-setter (gc-value plane-mask)))
(define set-gc-foreground! (make-gc-setter (gc-value foreground)))
(define set-gc-background! (make-gc-setter (gc-value background)))
(define set-gc-line-width! (make-gc-setter (gc-value line-width)))
(define set-gc-line-style! (make-gc-setter (gc-value line-style)))
(define set-gc-cap-style! (make-gc-setter (gc-value cap-style)))
(define set-gc-join-style! (make-gc-setter (gc-value join-style)))
(define set-gc-fill-style! (make-gc-setter (gc-value fill-style)))
(define set-gc-fill-rule! (make-gc-setter (gc-value fill-rule)))
(define set-gc-arc-mode! (make-gc-setter (gc-value arc-mode)))
(define set-gc-tile! (make-gc-setter (gc-value tile)))
(define set-gc-stipple! (make-gc-setter (gc-value stipple)))
(define set-gc-ts-x-origin! (make-gc-setter (gc-value ts-x-origin)))
(define set-gc-ts-y-origin! (make-gc-setter (gc-value ts-y-origin)))
(define set-gc-font! (make-gc-setter (gc-value font)))
(define set-gc-subwindow-mode! (make-gc-setter (gc-value subwindow-mode)))
(define set-gc-graphics-exposures!
  (make-gc-setter (gc-value graphics-exposures)))
(define set-gc-clip-x-origin! (make-gc-setter (gc-value clip-x-origin)))
(define set-gc-clip-y-origin! (make-gc-setter (gc-value clip-y-origin)))
(define set-gc-clip-mask! (make-gc-setter (gc-value clip-mask)))
(define set-gc-dash-offset! (make-gc-setter (gc-value dash-offset)))
(define set-gc-dashes! (make-gc-setter (gc-value dashes)))

(import-xlib-function get-gc-values (display gc values)
  "scx_Get_Gc_Values")

(define (make-gc-getter name)
  (lambda (display gc)
    (let ((values (get-gc-values display gc (list name))))
      (and values (cdr (assq name values))))))

(define gc-gc-function (make-gc-getter (gc-value function)))
(define gc-plane-mask (make-gc-getter (gc-value plane-mask)))
(define gc-foreground (make-gc-getter (gc-value foreground)))
(define gc-background (make-gc-getter (gc-value background)))
(define gc-line-width (make-gc-getter (gc-value line-width)))
(define gc-line-style (make-gc-getter (gc-value line-style)))
(define gc-cap-style (make-gc-getter (gc-value cap-style)))
(define gc-join-style (make-gc-getter (gc-value join-style)))
(define gc-fill-style (make-gc-getter (gc-value fill-style)))
(define gc-fill-rule (make-gc-getter (gc-value fill-rule)))
(define gc-arc-mode (make-gc-getter (gc-value arc-mode)))
(define gc-tile (make-gc-getter (gc-value tile)))
(define gc-stipple (make-gc-getter (gc-value stipple)))
(define gc-ts-x-origin (make-gc-getter (gc-value ts-x-origin)))
(define gc-ts-y-origin (make-gc-getter (gc-value ts-y-origin)))
(define gc-font (make-gc-getter (gc-value font)))
(define gc-subwindow-mode (make-gc-getter (gc-value subwindow-mode)))
(define gc-graphics-exposures (make-gc-getter (gc-value graphics-exposures)))
(define gc-clip-x-origin (make-gc-getter (gc-value clip-x-origin)))
(define gc-clip-y-origin (make-gc-getter (gc-value clip-y-origin)))
(define gc-clip-mask (make-gc-getter (gc-value clip-mask)))
(define gc-dash-offset (make-gc-getter (gc-value dash-offset)))
(define gc-dashes (make-gc-getter (gc-value dashes)))

(import-xlib-function free-gc (display gc)
  "scx_Free_Gc")

(import-lambda-definition gcontext-from-gc (gc)
  "scx_GContext_From_Gc")

;; *** GC convenience routines ***************************************

(define (set-line-attributes! display gc line-width line-style cap-style
			      join-style)
  (change-gc display gc
	     (make-gc-value-alist (line-width line-width)
				  (line-style line-style)
				  (cap-style cap-style)
				  (join-style join-style))))

(import-xlib-function set-dashes! (display gc dashoffset dashlist)
  "scx_Set_Dashes")

(define (set-clip-origin display gc x-origin y-origin)
  (change-gc display gc
	     (make-gc-value-alist (clip-x-origin x-origin)
				  (clip-y-origin y-origin))))

(define-enumerated-type rectangle-ordering :rectangle-ordering
  rectangle-ordering? rectangle-orderings
  rectangle-ordering-name rectangle-ordering-index
  (unsorted y-sorted xy-sorted xy-banded))

(define-exported-binding "scx-rectangle-ordering" :rectangle-ordering)
(define-exported-binding "scx-rectangle-orderings" rectangle-orderings)

;; rectangles has to be list of (x y width height) lists.
(import-xlib-function set-clip-rectangles!
  (display gc x-origin y-origin rectangles ordering)
  "scx_Set_Clip_Rectangles")

;; *** determine efficient sizes *************************************

;; returns a pair (width . height)
(import-lambda-definition %%query-best-size (screen class width height)
  "scx_Query_Best_Size")

(define (%query-best-size screen class width height)
  (call-xlib-function (screen:display screen) 'query-best-size
		      (lambda ()
			(%%query-best-size screen class width height))))

(define (query-best-cursor screen width height)
  (%query-best-size screen 0 width height))

(define (query-best-tile screen width height)
  (%query-best-size screen 1 width height))

(define (query-best-stipple screen width height)
  (%query-best-size screen 2 width height))
