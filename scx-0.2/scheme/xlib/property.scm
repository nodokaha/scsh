;; Copyright (c) 2001-2003 by David Frese

(define-record-type property :property
  (make-property type format data)
  property?
  (type property:type set-property:type!) ;; an atom
  (format property:format set-property:format!) ;; a property-format
  ;; a string if format is char, or an integer list otherwise
  (data property:data set-property:data!))

(define-exported-binding "scx-property" :property)

;; *** create or return atom names ***********************************

(import-xlib-function intern-atom (display atom-name only-if-exists?)
  "scx_Intern_Atom")

;; returns a list of atoms or #f
(import-xlib-function intern-atoms (display names only-if-exists?)
  "scx_Intern_Atoms")

(import-xlib-function get-atom-name (display atom)
  "scx_Get_Atom_Name")

(define (get-atom-names display atoms)
  (map (lambda (atom) (get-atom-name display atom)) atoms))

;; *** obtain and change window properties****************************

(import-xlib-function list-properties (display window)
  "scx_List_Properties")

;; Note: This does not change the list itself.
(import-xlib-function rotate-window-properties
  (display window properties npositions)
  "scx_Rotate_Window_Properties")

(import-xlib-function delete-property (display window property)
  "scx_Delete_Property")

;; returns a pair (bytes-after . property) or #f
(import-xlib-function get-window-property
  (display window atom offset length delete? req-type)
  "scx_Get_Window_Property")

(define-enumerated-type change-property-mode :change-property-mode
  change-property-mode? change-property-modes change-property-mode-name
  change-property-mode-index
  (replace prepend append))

(define-exported-binding "scx-change-property-mode" :change-property-mode)

(import-xlib-function change-property
  (display window atom mode property)
  "scx_Change_Property")

(define (get-full-window-property display window atom delete? req-type)
  (let ((res1 (get-window-property display window atom 0 0 #f req-type)))
    (and res1
	 (let ((res2 (get-window-property display window atom 0
					  (car res1) #f req-type)))
	   (and res2 (cdr res2))))))

;; separates a string at 0 characters and returns the bits in a list.
(define (string->string-list s)
  (let ((i (string-index s (ascii->char 0))))
    (if i
	(cons (substring s 0 i)
	      (string->string-list (substring s (+ i 1)
					      (string-length s))))
	(list s))))

(define (string-list->string strings)
  (if (null? strings)
      ""
      (fold (lambda (res s)
	      (string-append res (make-string 1 (ascii->char 0))
			     s))
	    (car strings)
	    (cdr strings))))

;; *** manipulate window selection ***********************************

(import-xlib-function set-selection-owner (display selection owner time)
  "scx_Set_Selection_Owner")

(import-xlib-function get-selection-owner (display selection)
  "scx_Get_Selection_Owner")

(import-xlib-function convert-selection
  (display selection target property requestor time)
  "scx_Convert_Selection")
