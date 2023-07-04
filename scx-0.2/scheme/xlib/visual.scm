;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

(define-record-type visual :visual
  (make-visual cpointer)
  visual?
  (cpointer visual:cpointer))

(define-exported-binding "scx-visual" :visual)

(define-enumerated-type visual-class :visual-class
  visual-class? visual-classes visual-class-name visual-class-index
  (static-gray gray-scale static-color pseudo-color true-color direct-color))

(define-exported-binding "scx-visual-class" :visual-class)
(define-exported-binding "scx-visual-classes" visual-classes)

(define-record-type visual-info :visual-info
  (make-visual-info visual visualid screen-number depth class red-mask
		    green-mask blue-mask colormap-size bits-per-rgb)
  visual-info?
  (visual visual-info:visual)
  (visualid visual-info:visualid set-visual-info:visualid!)
  (screen-number visual-info:screen-number set-visual-info:screen-number!)
  (depth visual-info:depth set-visual-info:depth!)
  (class visual-info:class set-visual-info:class!)
  (red-mask visual-info:red-mask set-visual-info:red-mask!)
  (green-mask visual-info:green-mask set-visual-info:green-mask!)
  (blue-mask visual-info:blue-mask set-visual-info:blue-mask!)
  (bits-per-rgb visual-info:bits-per-rgb set-visual-info:bits-per-rgb!)
  (colormap-size visual-info:colormap-size set-visual-info:colormap-size!))

(define-exported-binding "scx-visual-info" :visual-info)

(define (empty-visual-info)
  (make-visual-info #f #f #f #f #f #f #f #f #f #f))

;; *** obtain visual information *************************************

;; returns a list of visual-infos that match the visual-info
;; template. #f entries in the template are ignored. Use
;; (empty-visual-info) to create a visual-info with all entries set to
;; #f.
(import-xlib-function get-visual-infos (display template)
  "scx_Get_Visual_Info")

;; returns a visual-info or #f
(import-xlib-function match-visual-info (display screen-number depth class)
  "scx_Match_Visual_Info")

(import-lambda-definition visualid-from-visual (visual)
  "scx_VisualIDFromVisual")
