;; Some enumerated types for resources. They correspond to the
;; constants defined in X.h and resource.c  So don't change the order!

(define-enumerated-type Xresource :Xresource
  Xresource-type?
  Xresources
  Xresource-name
  Xresource-index
  (Unknown String Callbacklist Float Backing-Store Dimension
   Translation Position Bitmap Cardinal Accelerators Boolean
   Colormap Cursor Display Font GContext Fixnum Pixel Pixmap
   Character Widget Window))

(define (integer->resource-type i)
  (vector-ref resource-type i))

(define (resource-type->integer name)
  (resource-type name))

;; resource-conversion from c to scheme:

(define (intern-resource-to-scheme-type ident data)
  (cond
   ((= ident 0)        
    (error "Resource type unknown" intern-resource-to-scheme-type ident))
   ;; nothing to do for this values:
   ((or (= ident 1)    
	(= ident 3)    
	(= ident 5)    
	(= ident 7)    
	(= ident 9)   
	(= ident 11)   
	(= ident 17)   
	(= ident 20)   
	data)
   ((= ident 2)        
    ;;not implemented...
    (error "not implemented"))
   ((= ident 4)        
    (integer->backing-store-type data))
   ((= ident 6)        
    ;;n.i.
    (error "not implemented"))
   ((or (= ident 8)    
	(= ident 19))  
    (make-pixmap (car data) (make-display (cdr data) #f) #f))
   ((= ident 10)       
    ;;n.i.
    (error "not implemented"))
   ((= ident 12)       
    (make-colormap (car data) (make-display (cdr data) #f) #f))
   ((= ident 13)       
    (make-cursor (car data) (make-display (cdr data) #f) #f))
   ((= ident 14)       
    (make-display data #f))
   ((= ident 15)       
    (if (car (cdr data))
	(make-font #f #f (car data) (cdr (cdr data)) #f)
	(make-font #f (car data) #f (cdr (cdr data)) #f)))
   ((= ident 16)       
    (make-gcontext (car data) (make-display (cdr data)) #f) #f)
   ((= ident 18)       
    (make-pixel data #f #f))
   ((= ident 21)       
    (make-widget data #f))
   ((= ident 22)       
    (make-window (car data) (make-display (cdr data) #f) #f))
   (else
    (error "no valid resource-type" intern-resouce-to-scheme-type ident)))))

;; -----------------------------------------------------------------------------



(define-enumerated-type backing-store-type :backing-store-type
  backing-store-type?
  backing-stores
  backing-store-name
  backing-store-index
  (NotUseful WhenMapped Always))


(define (integer->backing-store-type i)
  (vector-ref backing-store-type i))

(define (backing-store-type->integer name)
  (backing-store-type name))