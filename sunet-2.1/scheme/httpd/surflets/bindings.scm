;; Copyright 2002, 2003 Andreas Bernauer

;; Bindings of POST requests can be read only once, since they are
;; read from an input port. So we have to cache them, for the case of
;; a later GET-BINDINGS call on the same POST request. The requests
;; are referenced by a weak pointer. Thread-safe as all threads use
;; the same lock.
(define *POST-bindings-cache* '())
(define *cache-lock* (make-lock))

(define (get-bindings surflet-request)
  (let ((request-method (surflet-request-method surflet-request))
	(content-type (assoc 'content-type
			     (surflet-request-headers surflet-request))))

    ;; Check if we the content-type is the one we support.  If there's
    ;; no content-type, assume the default (this is the one we
    ;; support).
    (if (and content-type
	     ;; Have to string-trim now, because the (buggy?) rfc822
	     ;; implementation leaves the leading whitespace of the
	     ;; header value.
	     (not (string=? (string-trim (cdr content-type))
			    "application/x-www-form-urlencoded")))
	(error "get-bindings currently only supports 
'application/x-www-form-urlencoded' as content-type"))
    (cond
     ((string=? request-method "GET")
      (form-query-list (http-url-search 
			(surflet-request-url surflet-request))))
     ((string=? request-method "POST")
      (or (cached-bindings surflet-request)
	  (let* ((content-length (get-content-length 
				  (surflet-request-headers surflet-request)))
		 (input-port (surflet-request-input-port surflet-request))
		 (form-data (read-string content-length input-port))
		 (form-bindings (form-query-list form-data)))
	    (obtain-lock *cache-lock*)
	    (set! *POST-bindings-cache* 
		  (cons (cons (make-weak-pointer surflet-request)
			      form-bindings)
			*POST-bindings-cache*))
	      (release-lock *cache-lock*)
	      form-bindings)))
     (else
      (error "unsupported request type")))))

;; Looking up, if we have cached this request. While going through the
;; list, we remove entries to request objects, that are no longer
;; valid. Expecting a call for an uncached request every now and then,
;; it is guaranteed, that the list is cleaned up every now and
;; then. The cache is a list of pairs
;;; (surflet-request . computed-binding)
(define (cached-bindings surflet-request)
  (obtain-lock *cache-lock*)
  (let ((result 
	 (let loop ((predecessor #f)
		    (cache *POST-bindings-cache*))
	     (if (null? cache)
		 #f			; no such request cached
		 (let* ((head (car cache))
			(s-req (weak-pointer-ref (car head))))
		   (if s-req
		       (if (eq? s-req surflet-request)
			   (cdr head)	; request is cached
			   (loop (if predecessor
				     (cdr predecessor)
				     cache)
				 (cdr cache))) ; request isn't cached
		       (begin     ;; request object is gone ==> remove
				  ;; it from list
			 (if predecessor
			     (set-cdr! predecessor (cdr cache))
			     (set! *POST-bindings-cache* (cdr cache)))
			 (loop predecessor
			       (cdr predecessor)))))))))
    (release-lock *cache-lock*)
    result))


;; Will be needed when we handle POST requests.
(define (get-content-length headers)
  (cond ((get-header headers 'content-length) =>
	 ;; adopted from httpd/cgi-server.scm
	 (lambda (content-length)	; Skip initial whitespace (& other non-digits).
	   (let ((first-digit (string-index content-length char-set:digit))
		 (content-length-len (string-length content-length)))
	     (if first-digit
		 (string->number (substring content-length first-digit 
					    content-length-len))
		 ;; (status-code bad-request) req 
		 (error "Illegal `Content-length:' header.")))))
	(else 
	 (error "No Content-length specified for POST data."))))

(define (extract-bindings key bindings)
  (let ((key (if (symbol? key) (symbol->string key) key)))
    (map cdr
	 (filter (lambda (binding) 
		   (equal? (car binding) key))
		 bindings))))

(define (extract-single-binding key bindings)
  (let ((key-bindings (extract-bindings key bindings)))
    (if (= 1 (length key-bindings))
	(car key-bindings)
	(error "extract-one-binding: more than one or zero bindings found"
	       (length key-bindings)
	       key bindings))))


