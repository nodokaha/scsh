;; *** create or destroy regions *************************************

(import-lambda-definition create-region () "scx_Create_Region")

(import-xlib-function set-region (display gc r)
  "scx_Set_Region")

(import-lambda-definition destroy-region (r) "scx_Destroy_Region")

;; *** determine if regions are empty or equal ***********************

(import-lambda-definition empty-region? (r) "scx_Empty_Region")

(import-lambda-definition equal-region? (r1 r2) "scx_Equal_Region")

(import-lambda-definition point-in-region? (r x y) "scx_Point_In_Region")

(define-enumerated-type rect-in-region-type :rect-in-region-type
  rect-in-region-type? rect-in-region-types rect-in-region-type-name
  rect-in-region-type-index
  (out in part))

(define-exported-binding "scx-rect-in-region-type" :rect-in-region-type)
(define-exported-binding "scx-rect-in-region-types" rect-in-region-types)

(import-lambda-definition rect-in-region? (r x y w h) "scx_Rect_In_Region")

;; *** region arithmetic *********************************************

;; intersect-region! computes the intersection of SRA and SRB and
;; stores the result in the region DR.

(import-lambda-definition intersect-region! (sra srb dr)
  "scx_Intersect_Region")

(define (intersect-region sra srb)
  (let ((dr (create-region)))
    (intersect-region! sra srb dr)
    dr))

(import-lambda-definition union-region! (sra srb dr)
  "scx_Union_Region")

(define (union-region sra srb)
  (let ((dr (create-region)))
    (union-region! sra srb dr)
    dr))

(import-lambda-definition union-rect-with-region! (x y w h src dest)
  "scx_Union_Rect_With_Region")

(define (union-rect-with-region x y w h src)
  (let ((dr (create-region)))
    (union-rect-with-region! x y w h src dr)
    dr))

(import-lambda-definition subtract-region! (sra srb dr)
  "scx_Subtract_Region")

(define (subtract-region sra srb)
  (let ((dr (create-region)))
    (subtract-region! sra srb dr)
    dr))

(import-lambda-definition xor-region! (sra srb dr)
  "scx_Xor_Region")

(define (xor-region sra srb)
  (let ((dr (create-region)))
    (xor-region! sra srb dr)
    dr))

(import-lambda-definition offset-region! (r dx dy)
  "scx_Offset_Region")

(import-lambda-definition shrink-region! (r dx dy)
  "scx_Shrink_Region")

;; *** generate regions **********************************************

;; points has to be a list of pairs (x . y).

(import-lambda-definition polygon-region (points fill-rule)
  "scx_Polygon_Region")

;; clip-box returns a list (x y width height)

(import-lambda-definition clip-box (r)
  "scx_Clip_Box")
