;; Copyright (c) 2001-2003 by David Frese

(define-record-type color :color
  (make-color pixel red green blue)
  color?
  (pixel color:pixel set-color:pixel!)
  (red color:red set-color:red!)
  (green color:green set-color:green!)
  (blue color:blue set-color:blue!))

(define-exported-binding "scx-color" :color)

(define-enumerated-type colormap-state :colormap-state
  colormap-state? colormap-states colormap-state-name colormap-state-index
  (uninstalled installed))

(define-exported-binding "scx-colormap-state" :colormap-state)
(define-exported-binding "scx-colormap-states" colormap-states)

;; *** create, copy, or destroy colormaps ****************************

(define-enumerated-type colormap-alloc :colormap-alloc
  colormap-alloc? colormap-allocs colormap-alloc-name colormap-alloc-index
  (none all))

(define-exported-binding "scx-colormap-alloc" :colormap-alloc)

(import-xlib-function create-colormap (display window visual alloc)
  "scx_Create_Colormap")

(import-xlib-function copy-colormap-and-free (display colormap)
  "scx_Copy_Colormap_And_Free")

(import-xlib-function free-colormap (display colormap)
  "scx_Free_Colormap")

;; *** allocate and free colors **************************************

(import-xlib-function alloc-color! (display colormap color)
  "scx_Alloc_Color")

;; red, green and blue can be a number between 0 (inclusive) and 1
;; (exclusive), or #f
(define (alloc-color display colormap red green blue)
  (let ((color (make-color 0 red green blue)))
    (and (alloc-color! display colormap color)
	 (color:pixel color))))

(import-xlib-function %alloc-named-color (display colormap color-name)
  "scx_Alloc_Named_Color")

;; returns a pair (screen-color exact-color) or #f
(define alloc-named-color/exact %alloc-named-color)

;; returns a color or #f
(define (alloc-named-color display colormap color-name)
  (let ((res (alloc-named-color/exact display colormap color-name)))
    (and res (car res))))

;; returns a pair of two lists (plane-masks . pixels) or #f
(import-xlib-function alloc-color-cells/planes
  (display colormap contig? nplanes npixels)
  "scx_Alloc_Color_Cells")

(define (alloc-color-cells display colormap contig? npixels)
  (let ((r (alloc-color-cells/planes display colormap contig? 0 npixels)))
    (and r (cdr r))))

;; returns a list of lists (pixels redmask greenmask bluemask) or #f
(import-xlib-function alloc-color-planes
  (display colormap contig? ncolors nreds ngreens nblues)
  "scx_Alloc_Color_Planes")

(import-xlib-function free-colors (display colormap pixels planes)
  "scx_Free_Colors")

;; *** obtain color values *******************************************

(import-xlib-function query-colors! (display colormap colors)
  "scx_Query_Colors")

(define (query-colors display colormap pixels)
  (let ((colors (map (lambda (pixel) (make-color pixel #f #f #f)) pixels)))
    (query-colors! display colormap colors)
    colors))

(define (query-color! display colormap color)
  (query-colors! display colormap (list color)))

(define (query-color display colormap pixel)
  (car (query-colors display colormap (list pixel))))

(import-xlib-function lookup-color (display colormap color-name)
  "scx_Lookup_Color")

(import-xlib-function parse-color (display colormap spec)
  "scx_Parse_Color")

;; *** set colors ****************************************************

(import-xlib-function store-colors (display colormap colors)
  "scx_Store_Colors")

(define (store-color display colormap color)
  (store-colors display colormap (list color)))

(import-xlib-function %store-named-color
  (display colormap color-name pixel do-red do-green do-blue)
  "scx_Store_Named_Color")

(define (store-named-color display colormap color-name pixel . args)
  (let ((flags (cond
		((null? args) '(#f #f #f))
		((= 3 (length args)) args)
		(else (error "invalid optional arguments" args))))) ;;TODO??
    (%store-named-color display colormap color-name pixel
			(car flags) (cadr flags) (caddr flags))))
