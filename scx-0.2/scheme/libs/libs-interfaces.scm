(define-interface xpm-interface
  (export create-pixmap-from-data
	  read-file-to-pixmap
	  ((xpm-attribute) :syntax)))

(define-interface xft-interface
  (export
   xft-pattern?
   make-xft-pattern
   xft-pattern-duplicate
   xft-pattern-get
   xft-pattern-add

   xft-font?
   xft-font-pattern
   xft-font-ascent
   xft-font-descent
   xft-font-height
   xft-font-max-advance-width

   xft-objectset?
   make-xft-objectset
   xft-objectset-add

   xft-draw?
   make-xft-draw
   make-xft-draw-bitmap
   xft-draw-display
   xft-draw-drawable
   xft-draw-colormap
   xft-draw-visual
   xft-draw-change

   xft-color?
   xft-color-alloc-name
   xft-color-alloc-value

   (xft-pattern-object :syntax)
   xft-pattern-object?
   xft-pattern-object-elements
   xft-pattern-object-name

   (xft-weight :syntax)
   xft-weight?
   xft-weight-elements
   xft-weight-name
   
   (xft-slant :syntax)
   xft-slant?
   xft-slant-elements
   xft-slant-name

   (xft-spacing :syntax)
   xft-spacing?
   xft-spacing-elements
   xft-spacing-name

   (xft-rgba :syntax)
   xft-rgba?
   xft-rgba-elements
   xft-rgba-name
   
   xft-font-match
   xft-font-open-pattern
   xft-font-open-name
   xft-font-open-xlfd

   xft-list-fonts-pattern-objects
   xft-text-extents-8bit
   xft-draw-string-8bit
   xft-draw-rect
   xft-draw-set-clip
   xft-pattern-print
   xft-default-has-render?
   
   xft-default-substitute

   xft-version-major
   xft-version-minor

   xft-result-match?
   xft-result-no-match?
   xft-result-type-mismatch?
   xft-result-no-id?))

(define-interface xrender-interface
  (export

   xglyphinfo?
   xglyphinfo-width
   xglyphinfo-height
   xglyphinfo-x
   xglyphinfo-y
   xglyphinfo-xOff
   xglyphinfo-yOff
   set-xglyphinfo-width!
   set-xglyphinfo-height!
   set-xglyphinfo-x!
   set-xglyphinfo-y!
   set-xglyphinfo-xOff!
   set-xglyphinfo-yOff!

   xrendercolor?
   xrendercolor-red
   xrendercolor-green
   xrendercolor-blue
   xrendercolor-alpha
   set-xrendercolor-red!
   set-xrendercolor-green!
   set-xrendercolor-blue!
   set-xrendercolor-alpha!))

(define-interface weak-table-interface
  (export
   make-value-weak-table
   weak-table?
   add-to-weak-table!
   remove-from-weak-table!
   lookup-in-weak-table))
