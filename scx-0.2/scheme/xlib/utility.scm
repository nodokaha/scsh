; The C-procedures for (xlib-release-X-or-later?) are in the
; file init.c

(import-lambda-definition xlib-release-4-or-later? ()
  "scx_Xlib_Release_4_Or_Later")			  

(import-lambda-definition xlib-release-5-or-later? ()
  "scx_Xlib_Release_5_Or_Later")

(import-lambda-definition xlib-release-6-or-later? ()
  "scx_Xlib_Release_6_Or_Later")

;; get-default returns the user default values of a specified program
;; from the X-resource database. program and option should be
;; strings. On success a string is returned, otherwise #f. See
;; XGetDefault.

(import-xlib-function get-default (display program option)
  "scx_Get_Default")

;; resource-manager-string returns the RESOURCE_MANAGER property from
;; the server's root window of screen 0, or #f if no such property
;; exists. See XResourceManagerString.

(import-xlib-function resource-manager-string (display)
  "scx_Resource_Manager_String")

;; parse-geometry parses a string for the standard X format for x, y,
;; width and height arguments. Definition:
;; [=][<width>{xX}<height>][{+-}<xoffset>{+-}<yoffset>]. The return
;; value is a list (x-negative? y-negative? x y width height), where
;; x, y, width, height can be #f if they were not specified in the
;; string. See XParseGeometry.

(define (parse-geometry string)
  (vector->list (%parse-geometry string)))

(import-lambda-definition %parse-geometry (string)
  "scx_Parse_Geometry")

;; these are some functions for clipboard handling.

(define store-buffer #f)
(define store-bytes #f)
(define fetch-buffer #f)
(define fetch-bytes #f)
(define rotate-buffers #f)

(let ((xa-string 31)    ;; from Xatom.h
      (xa-cut-buffers '(9 10 11 12 13 14 15 16)))
		     ;(9...16 are XA_CUT_BUFFER0...XA_CUT_BUFFER7)
  (set! store-buffer
	(lambda (dpy bytes buf)
	  (if (<= 0 buf 7)
	      (change-property dpy (default-root-window dpy)
			       (list-ref xa-cut-buffers buf)
			       (change-property-mode replace)
			       (make-property xa-string
					      8
					      bytes)))))

  (set! store-bytes (lambda (dpy bytes)
		      (store-buffer dpy bytes 0)))

  (set! fetch-buffer
	(lambda (dpy buf)
	  (if (<= 0 buf 7)
	      (let ((p (get-full-window-property
			dpy (default-root-window dpy)
			(list-ref xa-cut-buffers buf)
			#f xa-string)))
		(if (and p (eq? (property:type p) xa-string)
			 (string? (property:data p)))
		    (property:data p)
		    "")))))

  (set! fetch-bytes (lambda (dpy)
		      (fetch-buffer dpy 0)))

  (set! rotate-buffers (lambda (dpy delta)
			 (rotate-window-properties dpy
						   (default-root-window dpy)
						   xa-cut-buffers delta))))



  

