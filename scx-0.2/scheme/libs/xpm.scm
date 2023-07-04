; Access to the xpm library

(define-enumerated-type xpm-attribute :xpm-attribute
  xpm-attribute?
  xpm-attributes
  xpm-attribute-name
  xpm-attribute-index
  (visual colormap depth size hotspot char-per-pixel color-symbols rgb-filename
   infos return-pixels extensions exact-colors closeness rgb-closeness
   color-key color-table return-alloc-pixels alloc-close-colors bitmap-format
   alloc-color free-colors color-closure))

(define-enumerated-type bitmap-format :bitmap-format
  bitmap-format? bitmap-formats bitmap-format-name bitmap-format-index
  (xy-bitmap 
   bitmap-format-1 ;; means xy-pixmap, but is not allowed as a bitmap-format
   z-pixmap))

(define (integer->bitmap-format int)
  (vector-ref bitmap-formats int))

(define (bitmap-format->integer v)
  (bitmap-format-index v))

(define xpm-attribute-alist->integer+vector
  (make-enum-alist->integer+vector
   xpm-attributes xpm-attribute-index
   (lambda (v)
     (cond
      ((eq? v (xpm-attribute visual))
       visual-Xvisual)
      ((eq? v (xpm-attribute colormap))
       colormap-Xcolormap)
      ((eq? v (xpm-attribute depth))
       (lambda (x) x))
;      ((eq? v (xpm-attribute color-symbols))
;       (lambda (color-symbols)
;	 (list->vector
;	  (map (lambda (mapping)
;		 (list->vector
;		  (list (name->string (car mapping))
;			(name->string (cadr mapping))
;			(pixel-Xpixel (caddr mapping)))))
;	       color-symbols))))
      ((or (eq? v (xpm-attribute return-pixels))
	   (eq? v (xpm-attribute return-alloc-pixels)))
       (lambda (x) x))
      ((or (eq? v (xpm-attribute exact-colors))
	   (eq? v (xpm-attribute alloc-close-colors)))
       (lambda (x) (if x 1 0)))
      ((eq? v (xpm-attribute bitmap-format))
       bitmap-format->integer) ;; xypixmap not allowed
      (else (lambda (x)
	      (warn "attribute not supported" v)
	      (unspecific)))))))

(define (name->string obj)
  (if (symbol? obj)
      (symbol->string obj)
      obj))

(define (make-result display vec)
  (vector-set! vec 0 (make-pixmap (vector-ref vec 0) 
				  display #t))
  (vector-set! vec 3 (make-pixmap (vector-ref vec 3) 
				  display #t))
  (vector->list vec))

(define (create-pixmap-from-data drawable data xpm-attribute-alist)
  (let ((r (%create-pixmap-from-data
	    (display-Xdisplay (drawable-display drawable))
	    (drawable-Xobject drawable)
	    data
	    (xpm-attribute-alist->integer+vector xpm-attribute-alist))))
    (case r
      ((0) (error "Not enough memory!"))
      ((1) (error "Invalid XPM-File data." data))
      (else
       (make-result (drawable-display drawable) r)))))

;-> (pixmap (width . height) (x-hot . y-hot) shape-mask) 

(import-lambda-definition %create-pixmap-from-data
			  (Xdisplay Xdrawable data attribute-vector)
  "scx_Create_Pixmap_From_Data")

;(define (create-data-from-pixmap ...))

(define (read-file-to-pixmap drawable filename xpm-attribute-alist)
  (let ((r (%read-file-to-pixmap
	    (display-Xdisplay (drawable-display drawable))
	    (drawable-Xobject drawable)
	    filename
	    (xpm-attribute-alist->integer+vector xpm-attribute-alist))))
    (case r
      ((0) (error "Not enough memory!"))
      ((1) (error "Invalid XPM-File data." filename))
      ((2) (error "Open failed." filename))
      (else (make-result (drawable-display drawable) r)))))

;-> (pixmap (width . height) (x-hot . y-hot) shape-mask) 


(import-lambda-definition %read-file-to-pixmap
			  (Xdisplay Xdrawable filename attribute-vector)
  "scx_Read_File_To_Pixmap")
  

;(define (write-file-from-pixmap ...))
