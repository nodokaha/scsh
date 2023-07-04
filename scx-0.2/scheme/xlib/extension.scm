(define (list-extensions display)
  (vector->list (%list-extensions (display-Xdisplay display))))

(import-lambda-definition %list-extensions (Xdisplay)
  "scx_List_Extensions")

(define (query-extension display name)
  (let ((res (%query-extension (display-Xdisplay display)
			       (if (symbol? name)
				   (string->symbol name)
				   name))))
    (if res
	(vector->list res)
	res)))

(import-lambda-definition %query-extension (Xdisplay name)
  "scx_Query_Extension")
