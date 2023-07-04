;--- the widget-datatype ---

;; the free-field in the record-type indicates whether the widget is alive or
;; not. It's set by a XtNdestroyCallback (see callback.c)
;; Is's neccessary to know if the XtWidged is still there or not.

(define-record-type widget :widget
  (really-make-widget free Xwidget)
  widget?
  (free get-free set-free!)
  (Xwidget real-widget-Xwidget widget-set-Xwidget!))

(define (widget-Xwidget widget)
  (if (none-resource? widget)
      0
      (real-widget-Xwidget widget)))

(define (make-widget XWidget)
  (if (= 0 XWidget)
      none-resource
      (let ((maybe-widget (widget-list-find Xwidget)))
	(if maybe-widget
	    maybe-widget
	    (let ((widget (really-make-widget #f Xwidget)))
	      (add-finalizer! widget finalize-widget)
	      (atom-list-set! Xwidget widget)
	      widget)))))


;; finalize-widget is called, when the garbage collector removes the last
;; reference to the widget from the heap. Then we can savely close the 
;; widget and remove the weak-pointer from our list.

(define (finalize-widget widget)
  (let ((Xwidget (widget-Xwidget widget)))
    (widget-list-delete! Xwidget)))


;; All widget records need to be saved in a weak-list, to have only one record
;; for the same XLib widget

(define *weak-widget-list* (make-integer-table))

(define (atom-list-find Xwidget)
  (let ((r (table-ref *weak-widget-list* Xwidget)))
    (if r 
	(weak-pointer-ref r)
	r)))

(define (widget-list-set! Xwidget widget)
  (let ((p (make-weak-pointer widget)))
    (table-set! *weak-widget-list* Xwidget p)))

(define (widget-list-delete! Xwidget)
  (table-set! *weak-widget-list* Xwidget #f))