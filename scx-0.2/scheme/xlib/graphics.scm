;; Copyright (c) 2001-2003 by Norbert Frese, David Frese

;; *** copy areas ****************************************************

(import-xlib-function copy-area
  (display src dest gc src-x src-y width height dest-x dest-y)
  "scx_Copy_Area")

(import-xlib-function copy-plane
  (display src dest gc src-x src-y width height dest-x dest-y plane)
  "scx_Copy_Plane")

;; *** draw points ***************************************************

(define-enumerated-type coord-mode :coord-mode
  coord-mode? coord-modes coord-mode-name coord-mode-index
  (origin previous))

(define-exported-binding "scx-coord-mode" :coord-mode)

(import-xlib-function draw-point (display drawable gc x y)
  "scx_Draw_Point")

;; points has to be a list of (x . y) pairs
(import-xlib-function draw-points (display drawable gc points mode)
  "scx_Draw_Points")

;; *** draw lines, polygons ******************************************

(import-xlib-function draw-line (display drawable gc x1 y1 x2 y2)
  "scx_Draw_Line")

;; points has to be a list of (x . y) pairs
(import-xlib-function draw-lines (display drawable gc points mode)
  "scx_Draw_Lines")

(import-xlib-function draw-segments (display drawable gc segments)
  "scx_Draw_Segments")

(define-record-type segment :segment
  (make-segment x1 y1 x2 y2)
  segment?
  (x1 segment:x1 set-segment:x1!)
  (y1 segment:y1 set-segment:y1!)
  (x2 segment:x2 set-segment:x2!)
  (y2 segment:y2 set-segment:y2!))

(define-exported-binding "scx-segment" :segment)

;; *** draw rectangles ***********************************************

(import-xlib-function draw-rectangle
  (display drawable gc x y width height)
  "scx_Draw_Rectangle")

(define-record-type rectangle :rectangle
  (make-rectangle x y width height)
  rectangle?
  (x rectangle:x set-rectangle:x!)
  (y rectangle:y set-rectangle:y!)
  (width rectangle:width set-rectangle:width!)
  (height rectangle:height set-rectangle:height!))

(define-record-discloser :rectangle
  (lambda (r)
    `(Rectangle ,(rectangle:x r) ,(rectangle:y r)
		,(rectangle:width r) ,(rectangle:height r))))

(define-exported-binding "scx-rectangle" :rectangle)

(import-xlib-function draw-rectangles (display drawable gc rectangles)
  "scx_Draw_Rectangles")

;; *** draw arcs *****************************************************

(import-xlib-function draw-arc
  (display drawable gc x y width height angle1 angle2)
  "scx_Draw_Arc")

(define-record-type arc :arc
  (make-arc x y width height angle1 angle2)
  arc?
  (x arc:x set-arc:x!)
  (y arc:y set-arc:y!)
  (width arc:width set-arc:width!)
  (height arc:height set-arc:height!)
  (angle1 arc:angle1 set-arc:angle1!)
  (angle2 arc:angle2 set-arc:angle2!))

(define-exported-binding "scx-arc" :arc)

(import-xlib-function draw-arcs (display drawable gc arcs)
  "scx_Draw_Arcs")

;; *** fill rectangles, polygons, or arcs ****************************

(import-xlib-function fill-rectangle
  (display drawable gc x y width height)
  "scx_Fill_Rectangle")

(import-xlib-function fill-rectangles (display drawable gc rectangles)
  "scx_Fill_Rectangles")

(define-enumerated-type polygon-shape :polygon-shape
  polygon-shape? polygon-shapes polygon-shape-name polygon-shape-index
  (complex non-convex convex))

(define-exported-binding "scx-polygon-shape" :polygon-shape)

(import-xlib-function fill-polygon (display drawable gc points shape mode)
  "scx_Fill_Polygon")

(import-xlib-function fill-arc
  (display drawable gc x y width height angle1 angle2)
  "scx_Fill_Arc")

;; arcs has to be a list of (x y width height angle1 angle2) lists.
(import-xlib-function fill-arcs (display drawable gc arcs)
  "scx_Fill_Arcs")

;; *** auxiliary functions *******************************************

(define (bounds x1 y1 x2 y2)
  (make-rectangle x1 y1 (- x2 x1) (- y2 y1)))

(define (grow-rectangle r dw dh . maybe-centric?)
  (if (or (null? maybe-centric?) (not (car maybe-centric?)))
      (make-rectangle (rectangle:x r) (rectangle:y r)
		      (+ (rectangle:width r) dw)
		      (+ (rectangle:height r) dh))
      (make-rectangle (- (rectangle:x r) (quotient dw 2))
		      (- (rectangle:y r) (quotient dh 2))
		      (+ (rectangle:width r) dw)
		      (+ (rectangle:height r) dh))))

(define (move/resize-rectangle r dx dy dw dh)
  (make-rectangle (+ (rectangle:x r) dx)
		  (+ (rectangle:y r) dy)
		  (+ (rectangle:width r) dw)
		  (+ (rectangle:height r) dh)))
