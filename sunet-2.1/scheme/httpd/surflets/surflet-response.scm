;;; SURFLET-RESPONSE: Surflets are expected to return this object type.
;;; STATUS is the status code, an exact integer. See httpd/response.scm
;;;  e.g. (status-code ok)
;;; CONTENT-TYPE is a string, most probably "text/html".
;;; HEADERS is a (maybe empty) list of pairs of (string or symbol);
;;;   Additional headers to send, e.g. '(("Cache-Control" . "no-cache")) or
;;;   '((Cache-Control . "no-cache")) etc.
;;; DATA is either
;;; * a string
;;; * a list of strings
;;; This list maybe extended to vectors later.
(define-record-type surflet-response :surflet-response
  (make-surflet-response status content-type headers data)
  surflet-response?
  (status surflet-response-status)
  (content-type surflet-response-content-type)
  (headers surflet-response-headers)
  (data surflet-response-data))

;; Allowed type for the data field.
(define (valid-surflet-response-data? data)
  (or (string? data) (list? data)))

;; For debug purposes
(define (surflet-response->string surflet-response)
  (format #f "#{SUrflet-response Status: ~a Content-Type: ~s Headers: ~s~%~s~%"
	  (surflet-response-status surflet-response)
	  (surflet-response-content-type surflet-response)
	  (surflet-response-headers surflet-response)
	  (surflet-response-data surflet-response)))

