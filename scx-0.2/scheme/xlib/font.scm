;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

(define-enumerated-type font-direction :font-direction
  font-direction? font-directions font-direction-name font-direction-index
  (left-to-right right-to-left))

(define-exported-binding "scx-font-direction" :font-direction)
(define-exported-binding "scx-font-directions" font-directions)

(define-record-type char-struct :char-struct
  (make-char-struct lbearing rbearing width ascent descent attributes)
  char-struct?
  (lbearing char-struct:lbearing)
  (rbearing char-struct:rbearing)
  (width char-struct:width)
  (ascent char-struct:ascent)
  (descent char-struct:descent)
  (attributes char-struct:attributes))

(define-exported-binding "scx-char-struct" :char-struct)

(define-record-type font-struct :font-struct
  (make-font-struct cpointer
		    fid direction min-char-or-byte2 max-char-or-byte2
		    min-byte1 max-byte1 all-char-exist? default-char
		    properties min-bounds max-bounds per-char ascent descent)
  font-struct?
  ;; properties is an alist atom -> number
  ;; per-char is a vector of char-structs
  ;; min-bounds, max-bounds are a char-struct
  (cpointer font-struct:cpointer)
  (fid font-struct:fid)
  (direction font-struct:direction)
  (min-char-or-byte2 font-struct:min-char-or-byte2)
  (max-char-or-byte2 font-struct:max-char-or-byte2)
  (min-byte1 font-struct:min-byte1)
  (max-byte1 font-struct:max-byte1)
  (all-char-exist? font-struct:all-char-exist?)
  (default-char font-struct:default-char)
  (properties font-struct:properties)
  (min-bounds font-struct:min-bounds)
  (max-bounds font-struct:max-bounds)
  (per-char font-struct:per-char)
  (ascent font-struct:ascent)
  (descent font-struct:descent))

(define-exported-binding "scx-fontstruct" :font-struct)

;; *** load or unload fonts ******************************************

(import-xlib-function load-font (display name)
  "scx_Load_Font")

(import-xlib-function unload-font (display font)
  "scx_Unload_Font")

;; returns a font-struct record or #f
(import-xlib-function query-font (display font-id)
  "scx_Query_Font")

;; returns a font-struct record or #f
(import-xlib-function load-query-font (display name)
  "scx_Load_Query_Font")

(import-xlib-function free-font (display font-struct)
  "scx_Free_Font")

(define (get-font-property font-struct atom)
  (let ((a (assq atom (font-struct:properties font-struct)))) ;; assq ??
    (and a (cdr a))))

;; *** obtain or free font names and information *********************

(import-xlib-function list-fonts (display pattern maxnames)
  "scx_List_Fonts")

;; returns an alist mapping name -> font-struct
(import-xlib-function list-fonts-with-info (display pattern maxnames)
  "scx_List_Fonts_With_Info")

;; *** set or get the font search path *******************************

(import-xlib-function set-font-path (display directories)
  "scx_Set_Font_Path")

(import-xlib-function get-font-path (display)
  "scx_Get_Font_Path")

;; TODO: ??
;; calc-index calculates the array-position in XFontStruct.per_char by giving 
;; the character index which ranges between [font-min-byte2...font-max-byte2] 
;; for one-byte fonts or for two-byte fonts the lower 8 bits must be between
;; [font-min-byte1...font-max-byte1] and the higher 8 bits must be between
;; [font-min-byte2...font-max-byte2]. An error is raised if the index does not
;; fit into these boundaries.
;(define (calc-index font index)
;  (let ((min1 (font-min-byte1 font))
;	(max1 (font-max-byte1 font))
;	(min2 (font-min-byte2 font))
;	(max2 (font-max-byte2 font))
;	(check-bounds 
;	 (lambda (min max i s)
;	   (if (or (< i min)
;		   (> i max))
;	       (error (string-append s
;				     (number->string min)
;				     " and "
;				     (number->string max)
;				     "; given")
;		      index)))))
;    (if (and (= 0 min1) (= 0 max1))
;	;; two-byte font
;	(let ((b1 (bitwise-and index 255))
;	      (b2 (bitwise-and (arithmetic-shift index -8) 255)))
;	  (check-bounds min1 max1 b1
;			"expected an integer with lower 8 bits between ")
;	  (check-bounds min2 max2 b2
;			"expected an integer with higher 8 bits between ")
;	  (+ (* b1 (+ (- max2 min2) 1))
;	     b2))
;	;; one-byte font
;	(begin
;	  (check-bounds min2 max2 index
;			"expected an integer between ")
;	  index))))

