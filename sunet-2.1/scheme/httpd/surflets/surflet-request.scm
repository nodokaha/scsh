;; Copyright Andras Bernauer (2003)

;; somehow `extend' httpd-request 
(define-record-type surflet-request :surflet-request
  (make-surflet-request request input-port)
  surflet-request?
  (request surflet-request-request)
  (input-port surflet-request-input-port))

(define (make-fake-selector request-selector)
  (lambda (surflet-request)
    (request-selector (surflet-request-request surflet-request))))

(define surflet-request-method  (make-fake-selector request-method))
(define surflet-request-uri     (make-fake-selector request-uri))
(define surflet-request-url     (make-fake-selector request-url))
(define surflet-request-version (make-fake-selector request-version))
(define surflet-request-headers (make-fake-selector request-headers))
(define surflet-request-socket  (make-fake-selector request-socket))