
(define-record-type xglyphinfo :xglyphinfo
  (make-xglyphinfo c-pointer)
  xglyphinfo?
  (c-pointer xglyphinfo-c-pointer))

(define-exported-binding "xglyphinfo" :xglyphinfo)

(define-record-type xrendercolor :xrendercolor
  (really-make-xrender-color c-pointer)
  xrendercolor?
  (c-pointer xrendercolor-c-pointer))

;; XGlyphInfo

(import-lambda-definition xglyphinfo-width
			  (xglyphinfo)
			  "scx_xglyphinfo_width_get")

(import-lambda-definition xglyphinfo-height
			  (xglyphinfo)
			  "scx_xglyphinfo_height_get")

(import-lambda-definition xglyphinfo-x
			  (xglyphinfo)
			  "scx_xglyphinfo_x_get")

(import-lambda-definition xglyphinfo-y
			  (xglyphinfo)
			  "scx_xglyphinfo_y_get")

(import-lambda-definition xglyphinfo-xOff
			  (xglyphinfo)
			  "scx_xglyphinfo_xOff_get")

(import-lambda-definition xglyphinfo-yOff
			  (xglyphinfo)
			  "scx_xglyphinfo_yOff_get")

(import-lambda-definition set-xglyphinfo-width!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_width_set")

(import-lambda-definition set-xglyphinfo-height!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_height_set")

(import-lambda-definition set-xglyphinfo-x!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_x_set")

(import-lambda-definition set-xglyphinfo-y!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_y_set")

(import-lambda-definition set-xglyphinfo-xOff!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_xOff_set")

(import-lambda-definition set-xglyphinfo-yOff!
			  (xglyphinfo new-value)
			  "scx_xglyphinfo_yOff_set")

;; XRenderColor

(import-lambda-definition xrendercolor-red
			  (xrendercolor)
			  "scx_xrendercolor_red_get")

(import-lambda-definition xrendercolor-green
			  (xrendercolor)
			  "scx_xrendercolor_green_get")

(import-lambda-definition xrendercolor-blue
			  (xrendercolor)
			  "scx_xrendercolor_blue_get")

(import-lambda-definition xrendercolor-alpha
			  (xrendercolor)
			  "scx_xrendercolor_alpha_get")

(import-lambda-definition set-xrendercolor-red!
			  (xrendercolor new-value)
			  "scx_xrendercolor_red_set")

(import-lambda-definition set-xrendercolor-green!
			  (xrendercolor new-value)
			  "scx_xrendercolor_green_set")

(import-lambda-definition set-xrendercolor-blue!
			  (xrendercolor new-value)
			  "scx_xrendercolor_blue_set")

(import-lambda-definition set-xrendercolor-alpha!
			  (xrendercolor new-value)
			  "scx_xrendercolor_alpha_set")