;; --- widget functions ---

(define (help-maybe-symbol->string obj)
  (if (symbol? obj)
      (symbol->string obj)
      (if (string? obj)
	  obj
	  (error "no valide operand" help-maybe-symbol->string))))
  

(define (help-arglist-symbol->string arg-list)
  (letrec ((loop 
	    (lambda (new-list old-list)
	      (if (null? old-list)
					;should be save without (reverse list)
		  new-list
		  (loop (cons (help-maybe-symbol->string (car old-list))
			      (cons (cdar old-list)
				    new-list))
			(cddr old-list))))))
    (loop '() arg-list)))



(define (destroy-widget widget)
  (if (get-free widget)
      (error "widget is already destroyed" destroy-widget)
      (%destroy-widget (widget-Xwidget widget))))

(import-lambda-definition %destroy-widget (XWidget)
  "scxt_Destroy_Widget")


(define create-shell 
  (lambda (app-name app-class parent display . args)
    (if (get-free parent)
	(error "free widget" create-shell)
	(make-widget
	 (%create-shell (help-maybe-symbol->string app-name)
			(help-maybe-symbol->string app-class)
			(widget-Xwidget parent)
			(display-Xdisplay display)
			(help-arglist-symbol->string arg-list))))))

(import-lambda-definition %create-shell ()
  "scxt_Create_Shell")			 



(define create-widget
  (lambda args
    (apply int-create-widget #f args)))

(define create-managed-widget
  (lambda args
    (apply int-create-widget #t args)))


;; abstraction of create-widget(s) create-managed-widget(s)...
(define int-create-widget
  (lambda (managed? . args)
    (let* ((arg-list-length (length args))
	   (arg-list-even? (even? arg-list-length))
	   (num-args (if arg-list-even?
			 (/ (- arg-list-length 2) 2)
			 (/ (- arg-list-length 1) 2)))
	   (real-arg-list (if arg-list-even?
			      (cddr args)
			      (cdddr args)))
	   (widget-name (if arg-list-even)
			; this fun has to be implemented...
			(%class-name (car args))
                        (if (symbol? (car args))
			    (symbol->string (car args))
			    (car args)))
	   (widget-class (if arg-list-even?
			 (widgetClass-XwidgetClass (car args))
			 (widgetClass-XwidgetClass (cadr args))))
	   (parent (if arg-list-even?
		   (widget-Xwidget (cadr args))
		   (widget-Xwidget (caddr args))))
	   (new-widget (%create-widget widget-name widget-class parent 
				       real-arg-list num-args 
				       managed?)))
      (make-widget new-widget))))


(import-lambda-definition %create-widget (String, XWidgetClass, XWidget, 
					  args, Cardinal, flag)
  "scxt_Create_Widget")

 

(define (realize-widget widget)
  (if (get-free widget)
      (error "wrong widget" realize-widget)
      (%realize-widget (widget-Xwidget widget))))

(import-lambda-definition %realize-widget (XWidget)
  "sctx_Realize_Widget")			 



(define (unrealize-widget widget)
  (if (get-free widget)
      (error "wrong widget" unrealize-widget)
      (%unrealize-widget (widget-Xwidget widget))))

(import-lambda-definition %unrealize-widget (XWidget)
  "sctx_Unrealize_Widget")


(define (widget-realized? widget)
  (if (get-free widget)
      (error "wrong widget" widget-realized?)
      (%widget-realized? (widget-Xwidget widget))))

(import-lambda-definition %widget-realized? (XWidget)
  "sctx_Widget_Realized_P")


;; Attention! I don't know if the use of the finalizer in make-display
;; is correct.

(define (widget-display widget)
  (if (get-free widget)
      (error "wrong widget" widget-display)
      (make-display (%widget-display (widget-Xwidget widget)) #f)))

(import-lambda-definition %widget-display (XWidget)
  "sctx_Widget_Display")


;; Returns the parent widget...

(define (widget-parent widget)
  (if (get-free widget)
      (error "wrong widget" widget-parent)
      (%widget-parent (widget-Xwidget widget))))

(import-lambda-definition %widget-parent (Xwidget)
  "scxt_Widget_Parent")
			 
;; Returns the name of the widget as a string.
(define (widget-name widget)
  (if (get-free widget)
      (error "wrong widget" widget-name)
      (%widget-parent (widget-Xwidget widget))))

(import-lambda-definition %widget-name (Xwidget)
  "scxt_Widget_Name")			  


;; Returns the window associated with the widget
(define (widget->window widget)
  (if (get-free widget)
      (error "wrong widget" widget->window)
      (%widget->window (widget-Xwidget widget))))

(import-lambda-definition %widget->window (Xwidget)
  "scxt_Widget_To_Window")



;; (define (widget-window widget)) ?????


(define (widget-compsite? widget)
  (if (get-free widget)
      (error "freed widget" widget-composite?)
      (%widget-composite? (widget-Xwidget widget))))

(import-lambda-definition %widget-composite? (Xwidget)
  "scxt_Widget_Composite_P")


;; internal function:
(define (help-wlist-get-free widget-list)
  (letrec ((loop 
	    (lambda (tsil)
	      (cond 
	       ((null? tsil) #f)
	       ((get-free (car tsil)) #t)
	       (else
		(loop (cdr tsil)))))))
    (loop widget-list)))

;; int. function:
(define (help-wlist-widget-Xwidget widget-list)
  (letrec ((loop
	    (lambda (new-list old-list)
	      (if (null? old-list)
		  new-list
		  (loop (cons (widget-Xwidget (car old-list)) new-list)
			(cdr old-list))))))
    (loop '() widget-list)))

	      
(define magage-children
  (lambda widget-list
    (if (help-wlist-get-free widget-list)
	(error "free widget" manage-children)
	(%manage-children (help-wlist-widget-Xwidget widget-list)
			  (length widget-list)))))

(import-lambda-definition %manage-children (Xwidget-list list-length)
  "scxt_Manage_Children")


(define (manage-child widget)
  (manage-children widget))


(define (unmanage-child widget)
  (unmanage-children widget))


(define unmanage-children
  (lambda widget-list
    (if (help-wlist-get-free widget-list)
	(error "free widget" unmanage-children)
	(%unmanage-children (help-wlist-widget-Xwidget widget-list)
			    (length widget-list)))))

(import-lambda-definition %unmanage-children (Xwidget-list list-length)
  "scxt_Unmanage_Children")


(define (widget-managed? widget)
  (if (get-free widget)
      (error "free widget" widget-managed?)
      (%widget-managed? (widget-Xwidget widget))))

(import-lambda-definition %widget-managed? (Xwidget)
  "scxt_Widget_Managed_P")


(define (widget-class widget)
  (if (get-free widget)
      (error "free widget" widget-class)
      (%widget-class (widget-Xwidget widget))))

(import-lambda-definition %widget-class (Xwidget)
  "scxt_Widget_Class")


(define (widget-superclass widget)
  (if (get-free widget)
      (error "free widget" widget-superclass)
      (let ((res (%widget-superclass (widget-Xwidget widget))))
	(if res res 'none))))
	  
(import-lambda-definition %widget-superclass (Xwidget)
  "scxt_Widget_Superclass")


;; TODO: class parameter: check neccessary?
(define (widget-subclass? widget wclass)
  (if (get-free widget)
      (error "free widget" widget-subclass?)
      (%widget-subclass? (widget-Xwidget widget) (wclass-Xwclass wclass))))

(imoport-lambda-definition %widget-subclass? (Xwidget Xwclass)
  "scxt_Widget_Subclass_P")			   


(define (set-mapped-when-managed! widget managed?)
  (if (get-free widget)
      (error "free widget" set-mapped-when-managed!)
      (%set-mapped-when-managed! (widget-Xwidget widget) managed?)))

(import-lambda-definition %set-mapped-when-managed! (Xwindget bool)
  "scxt_Set_Mapped_When_Managed")			  


(define (map-widget widget)
  (if (get-free widget)
      (error "free widget" map-widget)
      (%map-widget (widget-Xwidget widget))))

(import-lambda-definition %map-widget (Xwidget)
  "scxt_Map_Widget")


(define (unmap-widget widget)
  (if (get-free widget)
      (error "free widget" unmap-widget)
      (%unmap-widget (widget-Xwidget widget))))

(import-lambda-definition %unmap-widget (Xwidget)
  "scxt_Unmap_Widget")


;; values consists of a widget and ressource arguments:
;; (the 1,3,5,.. a name to be set and the 2, 4, 6, ... the corresponding value)
(define set-values!
  (lambda values
    (let ((widget (car values))
	  (args (help-arg-list-symbol->string (cdr values))))
      (if (get-free widget)
	  (error "free widget" set-values!)
	  (%set-values! (widget-Xwidget widget) args (/ (length args) 2))))))
      
(import-lambda-definition %set-args (Xwidget arg-list num-args)
  "scxt_Set_Values")


;; the args are the wanted ressource names...
(define get-values
  (lambda values
    (let ((widget (car values))
	  (args (map help-maybe-string->symbol (cdr values))))
      (if (get-free widget)
	  (error "free widget" get-values)
	  (%get-values (widget-Xwidget widget) args (length args))))))
  

(import-lambda-definition %get-values (Xwidget arg-list num-args)
  "scxt_Get_Values")			  
			  


(define (widget-context widget)
  (if (get-free widget)
      (error "free widget" widget-context)
      (%widget-context (widget-Xwidget widget))))

(import-lambda-definition %widget-context (Xwidget)
  "scxt_Widget_Context")





(define (set-sensitive! widget sensitive?)
  (if (get-free widget)
      (error "free widget" set-sensitive!)
      (%set-sensitive! (widget-Xwidget widget) sensitive?)))

(import-lambda-definition %set-sensitive! (Xwidget boolean)
  "scxt_Set_Sensitive")





(define (widget-sensitive? widget)
  (if (get-free widget)
      (error "free widget" widget-sensitive?)
      (%widget-sensitive? (widget-Xwidget widget))))

(import-lambda-definition %widget-sensitive? (Xwidget)
  "scxt_Widget_Sensitive_P")			  




(define (window->widget window)
  (make-widget
   (%window->widget (window-Xwindow window)
		    (display-Xdisplay (window-display window)))))

(import-lambda-definition %window->widget (Xwindow Xdisplay)
  "scxt_Window_To_Widget")


;; returns the widget with the specified name and which is a
;; child of the root-widget. name is either a symbol or a string.
;; If no widget is found, NULL will be returned.
(define (name->widget root-widget name)
  (if (get-free root-widget)
      (error "free widget" name->widget)
      (make-widget
       (%name->widget (widget-Xwidget root-widget)
		      (help-maybe-symbol->string name)))))

(import-lambda-definition %name->widget (Xwidget string)
  "scxt_Name_To_Widget")


;; returns a pair of integers, which represent the coordinates
;; relative to it's root-widget.
(define (widget-translate-coordinates widget x y)
  (if (get-free widget)
      (error "free widget" widget-translate-coordinates)
      (%widget-translate-coordinates (widget-Xwidget widget) x y)))
       
(import-lambda-definition %widget-translate-coordinates (Xwidget int-x int-y)
  "scxt_Widget_Translate_Coordinates")
