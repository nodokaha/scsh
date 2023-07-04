;; Copyright (c) 2001-2003 by Norbert Frese, David Frese

;; *** create or destroy pixmaps *************************************

(import-xlib-function create-pixmap (display drawable width height depth)
  "scx_Create_Pixmap")

(import-xlib-function free-pixmap (display pixmap)
  "scx_Free_Pixmap")

;; *** manipulate bitmaps ********************************************

(define (bitmap-error i data) ;; TODO exceptions ?!
  (case i
    ((0) #t) ;; no error
    ((1) (error "could not open file" data))
    ((2) (error "invalid bitmap data in file" data))
    ((3) (error "not enough memory to create bitmap" data))))

(import-xlib-function %read-bitmap-file (display drawable filename)
  "scx_Read_Bitmap_File")

;; returns a list (pixmap width height x-hot y-hot). May raise an error.
(define (read-bitmap-file display drawable filename)
  (let ((res (%read-bitmap-file display drawable filename)))
    (if (number? res)
	(bitmap-error res filename)
	res)))

(import-xlib-function %write-bitmap-file
  (display filename bitmap width height x-hot y-hot)
  "scx_Write_Bitmap_File")

(define (write-bitmap-file display filename bitmap width height x-hot y-hot)
  (bitmap-error (%write-bitmap-file display filename bitmap width height
				    x-hot y-hot)
		filename))

;; create-bitmap-from-data creates a new pixmap, consisting of the
;; image found in data, which has to be a string. Such an image can be
;; generated with write-bitmap-file. See XCreateBitmapFromData.

(import-xlib-function create-bitmap-from-data (display drawable data w h)
  "scx_Create_Bitmap_From_Data")

;; create-pixmap-from-bitmap-data creates a pixmap of the given depth
;; and then does a bitmap-format XPutImage of the data into it. See
;; XCreatePixmapFromBitmapData.

(import-xlib-function create-pixmap-from-bitmap-data
  (display drawable data width height foreground background depth)
  "scx_Create_Pixmap_From_Bitmap_Data")

