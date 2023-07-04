
;; *object-table* maps each Xft object (identified by its memory
;; address) to the correspoding Scheme object.  This avoids situations
;; where we have two pointer Scheme objects for one Xft object.  In
;; these situations we would call the finalizer (Xft free() operation)
;; on twice on the same object.  This may crash the system, or, at
;; least print bogus error messages.
;; Key is an integer (memory address of the Xft structure), value a
;; weak pointer to the Scheme object.

(define *object-table*
  (make-value-weak-table))

(define (register-xft-pointer! pointer object)
  (add-to-weak-table! *object-table* pointer object))

(define (unregister-xft-pointer! pointer)
  (remove-from-weak-table! *object-table* pointer))

(define (lookup-xft-pointer pointer)
  (lookup-in-weak-table *object-table* pointer))

;; types

(define-record-type xft-pattern :xft-pattern
  (really-make-xft-pattern c-pointer)
  xft-pattern?
  (c-pointer xft-pattern-c-pointer))

(define-exported-binding "xft-pattern" :xft-pattern)

(define-record-type xft-font :xft-font
  (make-xft-font c-pointer pattern)
  xft-font?
  (c-pointer xft-font-c-pointer)
  (pattern xft-font-pattern)
  (display xft-font-display))

(define-exported-binding "xft-font" :xft-font)

(define-record-type xft-draw :xft-draw
  (make-xft-draw-internal c-pointer)
  xft-draw?
  (c-pointer xft-draw-c-pointer))

(define-exported-binding "xft-draw" :xft-draw)

(define-record-type xft-color :xft-color
  (make-xft-color-internal c-pointer display visual colormap)
  xft-color?
  (c-pointer xft-color-c-pointer)
  (display xft-color-display set-xft-color-display!)
  (visual xft-color-visual set-xft-color-visual!)
  (colormap xft-color-colormap set-xft-color-colormap!))

(define-exported-binding "xft-color" :xft-color)

(define-record-type xft-objectset :xft-objectset
  (make-xft-objectset c-pointer)
  xft-objectset?
  (c-pointer xft-objectset-c-pointer))

(define-exported-binding "xft-objectset" :xft-objectset)

(define-record-type xft-fontset :xft-fontset
  (really-make-xft-fontset c-pointer)
  xft-fontset?
  (c-pointer xft-fontset-c-pointer))

(define-exported-binding "xft-fontset" :xft-fontset)

(define-syntax lookup-shared-valued
  (syntax-rules ()
    ((lookup-shared-valued str)
     (shared-binding-ref
      (lookup-imported-binding str)))))
 
(define-finite-type xft-pattern-object :xft-pattern-object
  (id)
  xft-pattern-object?
  xft-pattern-object-elements
  xft-pattern-object-name
  xft-pattern-object-index
  (id xft-pattern-object-id)
  ((family	(lookup-shared-valued "scx-xft-pattern-family"))
   (style	(lookup-shared-valued "scx-xft-pattern-style"))
   (slant	(lookup-shared-valued "scx-xft-pattern-slant"))
   (weight	(lookup-shared-valued "scx-xft-pattern-weight"))
   (size	(lookup-shared-valued "scx-xft-pattern-size"))
   (pixel-size	(lookup-shared-valued "scx-xft-pattern-pixel-size"))
   (encoding	(lookup-shared-valued "scx-xft-pattern-encoding"))
   (spacing	(lookup-shared-valued "scx-xft-pattern-spacing"))
   (foundry	(lookup-shared-valued "scx-xft-pattern-foundry"))
   (core	(lookup-shared-valued "scx-xft-pattern-core"))
   (antialias	(lookup-shared-valued "scx-xft-pattern-antialias"))
   (xlfd	(lookup-shared-valued "scx-xft-pattern-xlfd"))
   (file	(lookup-shared-valued "scx-xft-pattern-file"))
   (index	(lookup-shared-valued "scx-xft-pattern-index"))
   (rasterizer	(lookup-shared-valued "scx-xft-pattern-rasterizer"))
   (outline	(lookup-shared-valued "scx-xft-pattern-outline"))
   (scalable	(lookup-shared-valued "scx-xft-pattern-scalable"))
   (rgba	(lookup-shared-valued "scx-xft-pattern-rgba"))
   (scale	(lookup-shared-valued "scx-xft-pattern-scale"))
   (render	(lookup-shared-valued "scx-xft-pattern-render"))
   (minspace	(lookup-shared-valued "scx-xft-pattern-minspace"))
   (dpi		(lookup-shared-valued "scx-xft-pattern-dpi"))
   (char-width	(lookup-shared-valued "scx-xft-pattern-char-width"))
   (char-height (lookup-shared-valued "scx-xft-pattern-char-height"))))

(define-finite-type xft-weight :xft-weight
  (id)
  xft-weight?
  xft-weight-elements
  xft-weight-name
  xft-weight-index
  (id xft-weight-id)
  ((light	(lookup-shared-valued "scx-xft-weight-light"))
   (medium	(lookup-shared-valued "scx-xft-weight-medium"))
   (demibold	(lookup-shared-valued "scx-xft-weight-demibold"))
   (bold	(lookup-shared-valued "scx-xft-weight-bold"))
   (black	(lookup-shared-valued "scx-xft-weight-black"))))

(define-finite-type xft-slant :xft-slant
  (id)
  xft-slant?
  xft-slant-elements
  xft-slant-name
  xft-slant-index
  (id xft-slant-id)
  ((roman	(lookup-shared-valued "scx-xft-slant-roman"))
   (italic	(lookup-shared-valued "scx-xft-slant-italic"))
   (oblique	(lookup-shared-valued "scx-xft-slant-oblique"))))

(define-finite-type xft-spacing :xft-spacing
  (id)
  xft-spacing?
  xft-spacing-elements
  xft-spacing-name
  xft-spacing-index
  (id xft-spacing-id)
  ((proportional	(lookup-shared-valued "scx-xft-spacing-proportional"))
   (mono		(lookup-shared-valued "scx-xft-spacing-mono"))
   (charcell		(lookup-shared-valued "scx-xft-spacing-charcell"))))

(define-finite-type xft-rgba :xft-rgba
  (id)
  xft-rgba?
  xft-rgba-elements
  xft-rgba-name
  xft-rgba-index
  (id xft-rgba-id)
  ((none	(lookup-shared-valued "scx-xft-rgba-none"))
   (rgb		(lookup-shared-valued "scx-xft-rgba-rgb"))
   (bgr		(lookup-shared-valued "scx-xft-rgba-bgr"))
   (vrgb	(lookup-shared-valued "scx-xft-rgba-vrgb"))
   (vbgr	(lookup-shared-valued "scx-xft-rgba-vbgr"))))

(define (make-finite-type-alist elements id-proc)
  (map (lambda (e) 
	 (cons (id-proc e) e))
       (vector->list elements)))
       
(define xft-weight-id->xft-weight
  (let ((alist 
	 (make-finite-type-alist xft-weight-elements xft-weight-id)))
    (lambda (id)
      (cond 
       ((assoc id alist) => cdr)
       (else 
	(error "scx: internal error. Could not map weight id to finite type"))))))

(define xft-slant-id->xft-slant
  (let ((alist 
	 (make-finite-type-alist xft-slant-elements xft-slant-id)))
    (lambda (id)
      (cond
       ((assoc id alist) => cdr)
       (else
	(error "scx: internal error. Could not map slant id to finite type"))))))

(define xft-spacing-id->xft-spacing
  (let ((alist 
	 (make-finite-type-alist xft-spacing-elements xft-spacing-id)))
    (lambda (id)
      (cond
       ((assoc id alist) => cdr)
       (else
	(error "scx: internal error. Could not map spacing id to finite type"))))))

(define xft-rgba-id->xft-rgba
  (let ((alist
	 (make-finite-type-alist xft-rgba-elements xft-rgba-id)))
    (lambda (id)
      (cond
       ((assoc id alist) => cdr)
       (else
	(error "scx: internal error. Could not map rgba id to finite type"))))))

(define (xft-pattern-finalizer pattern)
  (unregister-xft-pointer! (xft-pattern-c-pointer pattern))
  (xft-pattern-destroy pattern))

(define (xft-color-finalizer color)
  (unregister-xft-pointer! (xft-color-c-pointer color))
  (xft-color-free (xft-color-display color)
		  (xft-color-visual color)
 		  (xft-color-colormap color)
 		  color))

(define (xft-draw-finalizer draw)
  (unregister-xft-pointer! (xft-draw-c-pointer draw))
  (xft-draw-destroy draw))

(define (xft-objectset-finalizer objectset)
  (unregister-xft-pointer! (xft-objectset-c-pointer objectset))
  (xft-objectset-destroy objectset))

(define (xft-font-finalizer font)
  (unregister-xft-pointer! (xft-font-c-pointer font))
  (xft-font-close-internal font))

(define (make-xft-pattern)
  (let ((pattern (xft-pattern-create)))
    (or (lookup-xft-pointer (xft-pattern-c-pointer pattern))
	(begin
	  (register-xft-pointer! (xft-pattern-c-pointer pattern) pattern)
	  (add-finalizer! pattern xft-pattern-finalizer)
	  pattern))))

(define (xft-pattern-duplicate pattern)
  (let* ((copy (xft-pattern-duplicate-internal pattern))
	 (pointer (xft-pattern-c-pointer copy)))
    (or (lookup-xft-pointer pointer)
	(begin
	  (register-xft-pointer! pointer copy)
	  (add-finalizer! copy xft-pattern-finalizer)
	  copy))))

(define (xft-pattern-get pattern object id)
  (let ((object-id (xft-pattern-object-id object)))
    (call-with-values
     (lambda ()
       (apply values 
	      (xft-pattern-get-internal pattern object-id id)))
     (lambda (code value)
       (values code
	       (cond
		((not (xft-result-match? code))
		 #f)
		((equal? object (xft-pattern-object weight))
		 (xft-weight-id->xft-weight value))
		((equal? object (xft-pattern-object slant))
		 (xft-slant-id->xft-slant value))
		((equal? object (xft-pattern-object spacing))
		 (xft-spacing-id->xft-spacing value))
		((equal? object (xft-pattern-object rgba))
		 (xft-rgba-id->xft-rgba value))
		(else value)))))))
	   
(define (xft-pattern-add pattern object value append?)
  (let* ((object-id (xft-pattern-object-id object))
	 (call (lambda (value)
		 (xft-pattern-add-internal 
		  pattern object-id value append?))))
    (cond
     ((equal? object (xft-pattern-object weight))
      (call (xft-weight-id value)))
     ((equal? object (xft-pattern-object slant))
      (call (xft-slant-id value)))
     ((equal? object (xft-pattern-object spacing))
      (call (xft-spacing-id value)))
     ((equal? object (xft-pattern-object rgba))
      (call (xft-rgba-id value)))
     (else (call value)))))

(define (xft-font-match display screen pattern)
  (call-with-values
   (lambda ()
     (let ((screen-number (screen:number screen)))
       (apply values
	      (xft-font-match-internal display screen-number pattern))))
   (lambda (result pattern)
     (let ((pointer (xft-pattern-c-pointer pattern)))
       (cond 
	((lookup-xft-pointer pointer)
	 => (lambda (obj) (values result obj)))
	(else
	 (register-xft-pointer! (xft-pattern-c-pointer pattern) pattern)
	 ;(add-finalizer! pattern xft-pattern-finalizer)
	 (values result pattern)))))))

(define (xft-font-open-pattern display pattern)
  (let ((font (xft-font-open-pattern-internal display pattern)))
    (or (lookup-xft-pointer (xft-font-c-pointer font))
	(begin
	  (register-xft-pointer! (xft-font-c-pointer font) font)
	  (add-finalizer! font xft-font-finalizer)
	  font))))

(define (xft-font-open-name display screen name)
  (let* ((screen-number (screen:number screen))
	 (font (xft-font-open-name-internal display screen-number name)))
    (or (lookup-xft-pointer (xft-font-c-pointer font))
	(begin
	  (register-xft-pointer! (xft-font-c-pointer font) font)
	  (add-finalizer! font xft-font-finalizer)
	  font))))

(define (xft-font-open-xlfd display screen name)
  (let* ((screen-numer (screen:number screen))
	 (font (xft-font-open-xlfd-internal display screen-numer name)))
    (or (lookup-xft-pointer (xft-font-c-pointer font))
	(begin
	  (register-xft-pointer! (xft-font-c-pointer font) font)
	  (add-finalizer! font xft-font-finalizer)
	  font))))

(define (make-xft-draw display drawable visual colormap)
  (let ((draw (xft-draw-create-internal display drawable visual colormap)))
    (or (lookup-xft-pointer (xft-draw-c-pointer draw))
	(begin
	  (register-xft-pointer! (xft-draw-c-pointer draw) draw)
	  (add-finalizer! draw xft-draw-finalizer)
	  draw))))

(define (make-xft-draw-bitmap display drawable)
  (let ((draw (xft-draw-create-bitmap-internal display drawable)))
    (or (lookup-xft-pointer (xft-draw-c-pointer draw))
	(begin
	  (register-xft-pointer! (xft-draw-c-pointer draw) draw)
	  (add-finalizer! draw xft-draw-finalizer)
	  draw))))

(define (make-xft-objectset)
  (let ((objectset (xft-objectset-create)))
    (or (lookup-xft-pointer (xft-objectset-c-pointer objectset))
	(begin
	  (register-xft-pointer! (xft-objectset-c-pointer objectset)
				 objectset)
	  (add-finalizer! objectset xft-objectset-finalizer)
	  objectset))))

(define (xft-objectset-add objectset pattern-object)
  (xft-objectset-add-internal 
   objectset (xft-pattern-object-id pattern-object)))

(define (xft-draw-display draw)
  (let ((display (xft-draw-display-internal draw)))
    (or display
	(error "XftDrawDisplay() unavailable in this version of Xft"))))

(define (xft-draw-drawable draw)
  (let ((drawable (xft-draw-drawable-internal draw)))
    (or drawable
	(error "XftDrawDrawable() unavailable in this version of Xft"))))

(define (xft-draw-colormap draw)
  (let ((colormap (xft-draw-colormap-internal draw)))
    (or colormap
	(error "XftDrawColormap() unavailable in this version of Xft"))))

(define (xft-draw-visual draw)
  (let ((visual (xft-draw-visual-internal draw)))
    (or visual
	(error "XftDrawVisual() unavailable in this version of Xft"))))

(define (fontset->list-of-patterns fs)
  (let ((count (xft-fontset-count fs)))
    (unfold
     (lambda (x) 
       (equal? count x))
     (lambda (index)
       (xft-pattern-duplicate
	(xft-fontset-ref fs index)))
     (lambda (x) 
       (+ x 1))
     0)))

(define (xft-list-fonts-pattern-objects display screen pattern objectset)
  (let* ((screen-number (screen:number screen))
	 (fontset (xft-list-fonts-pattern-objects-internal
		   display screen-number pattern objectset)))
    (let ((patterns (fontset->list-of-patterns fontset)))
      (xft-fontset-destroy fontset)
      patterns)))

(define (xft-color-alloc-name display visual colormap name)
  (call-with-values
   (lambda ()
     (apply values
	    (xft-color-alloc-name-internal display visual colormap name)))
   (lambda (success? color)
     (if success?
	 (begin
	   (set-xft-color-display! color display)
	   (set-xft-color-visual! color visual)
	   (set-xft-color-colormap! color colormap)
	   (add-finalizer! color xft-color-finalizer)
	   color)
	 ;;; FIXME: raise error
	 #f))))

(define (xft-color-alloc-value display visual colormap xrendercolor)
  (call-with-values
   (lambda () 
     (apply values
	    (xft-color-alloc-value-internal display visual colormap xrendercolor)))
   (lambda (success? color)
     (set-xft-color-display! color display)
     (set-xft-color-visual! color visual)
     (set-xft-color-colormap! color colormap)
     (add-finalizer! color xft-color-finalizer)
     color)))

(define (xft-default-substitute display screen pattern)
  (xft-default-substitute-internal display (screen:number screen) pattern))

;;; import values from C code
(define xft-version-major
  (shared-binding-ref
   (lookup-imported-binding "scx-xft-version-major")))

(define xft-version-minor
  (shared-binding-ref
   (lookup-imported-binding "scx-xft-version-minor")))

(define xft-result-match?
  (let ((code (shared-binding-ref
	       (lookup-imported-binding "scx-xft-result-match"))))
    (lambda (value)
      (equal? value code))))

(define xft-result-no-match?
  (let ((code (shared-binding-ref
	       (lookup-imported-binding "scx-xft-result-no-match"))))
    (lambda (value)
      (equal? value code))))

(define xft-result-type-mismatch?
  (let ((code (shared-binding-ref
	       (lookup-imported-binding "scx-xft-result-type-mismatch"))))
    (lambda (value)
      (equal? value code))))

(define xft-result-no-id?
  (let ((code (shared-binding-ref
	       (lookup-imported-binding "scx-xft-result-no-id"))))
    (lambda (value)
      (equal? value code))))

;;; import functions from C code

(import-lambda-definition xft-pattern-create 
			  ()
			  "scx_XftPatternCreate")

(import-lambda-definition xft-pattern-destroy
			  (pattern)
			  "scx_XftPatternDestroy")

(import-lambda-definition xft-pattern-duplicate-internal
			  (pattern)
			  "scx_XftPatternDuplicate")

(import-lambda-definition xft-pattern-get-internal
			  (pattern object id)
			  "scx_XftPatternGet")

(import-lambda-definition xft-pattern-add-internal
			  (pattern object value append?)
			  "scx_XftPatternAdd")

(import-lambda-definition xft-font-match-internal
			  (display screen-number pattern)
			  "scx_XftFontMatch")

(import-lambda-definition xft-font-open-pattern-internal
			  (display pattern)
			  "scx_XftFontOpenPattern")

(import-lambda-definition xft-font-open-name-internal
			  (display screen-number name)
			  "scx_XftFontOpenName")

(import-lambda-definition xft-font-open-xlfd-internal
			  (display screen-number xlfd-name)
			  "scx_XftFontOpenXlfd")

(import-lambda-definition xft-font-close-internal
			  (display font)
			  "scx_XftFontClose")

(import-lambda-definition xft-draw-create-internal
			  (display drawable visual colormap)
			  "scx_XftDrawCreate")

(import-lambda-definition xft-draw-create-bitmap-internal
			  (display drawable)
			  "scx_XftDrawCreateBitmap")

(import-lambda-definition xft-draw-change
			  (draw drawable)
			  "scx_XftDrawChange")

(import-lambda-definition xft-draw-display-internal
			  (draw)
			  "scx_XftDrawDisplay")

(import-lambda-definition xft-draw-drawable-internal
			  (draw)
			  "scx_XftDrawDrawable")

(import-lambda-definition xft-draw-colormap-internal
			  (draw)
			  "scx_XftDrawColormap")

(import-lambda-definition xft-draw-visual-internal
			  (draw)
			  "scx_XftDrawVisual")

(import-lambda-definition xft-draw-destroy
			  (draw)
			  "scx_XftDrawDestroy")

(import-lambda-definition xft-text-extents-8bit
			  (display font string)
			  "scx_XftTextExtents8")

(import-lambda-definition xft-draw-string-8bit
			  (draw color font x y string)
			  "scx_XftDrawString8")

(import-lambda-definition xft-draw-rect
			  (draw color x y w h)
			  "scx_XftDrawRect")

(import-lambda-definition xft-draw-set-clip
			  (draw region)
			  "scx_XftDrawSetClip")

(import-lambda-definition xft-objectset-create
			  ()
			  "scx_XftObjectSetCreate")

(import-lambda-definition xft-objectset-destroy
			  (objectset)
			  "scx_XftObjectSetDestroy")

(import-lambda-definition xft-objectset-add-internal
			  (objectset object)
			  "scx_XftObjectSetAdd")

(import-lambda-definition xft-list-fonts-pattern-objects-internal
			  (display screen-number pattern objectset)
			  "scx_XftListFontsPatternObjects")

(import-lambda-definition xft-fontset-destroy
			  (fontset)
			  "scx_XftFontSetDestroy")

(import-lambda-definition xft-color-alloc-name-internal
			  (display visual colormap name)
			  "scx_XftColorAllocName")

(import-lambda-definition xft-color-alloc-value-internal
			  (display visual colormap xrendercolor)
			  "scx_XftColorAllocValue")

(import-lambda-definition xft-color-free
			  (display visual colormap color)
			  "scx_XftColorFree")

(import-lambda-definition xft-pattern-print
			  (pattern)
			  "scx_XftPatternPrint")

(import-lambda-definition xft-default-has-render?
			  (display)
			  "scx_XftDefaultHasRender")

(import-lambda-definition xft-default-substitute-internal
			  (display screen-number pattern)
			  "scx_XftDefaultSubstitute")

(import-lambda-definition xft-font-ascent
			  (font)
			  "scx_xftfont_ascent_get")

(import-lambda-definition xft-font-descent
			  (font)
			  "scx_xftfont_descent_get")

(import-lambda-definition xft-font-height
			  (font)
			  "scx_xftfont_height_get")

(import-lambda-definition xft-font-max-advance-width
			  (font)
			  "scx_xftfont_max_advance_width_get")

(import-lambda-definition xft-fontset-count
			  (fontset)
			  "scx_xftfontset_count_get")

(import-lambda-definition xft-fontset-ref
			  (fontset index)
			  "scx_xftfontset_pattern_ref")
