;; Pretty-printing of IPv4 Internet addresses

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;; ADDRESS    address to pretty-print
;; SEPERATOR  optional, defaults to ".", seperator between address-parts
;; Example:
;; (format-internet-host-address #x0a00ffff)
;; ==> "10.0.255.255"
;; (format-internet-host-address #x0a00ffff ":")
;; ==> "10:0:255:255"

(define (format-internet-host-address address . maybe-separator)

  (let ((extract (lambda (shift)
		   (number->string
		    (bitwise-and (arithmetic-shift address (- shift))
				 255)))))
		 
    (let-optionals maybe-separator ((separator "."))
		   (string-append
		    (extract 24) separator (extract 16) separator
		    (extract 8) separator (extract 0)))))
  
;; does pretty-print of ports
;; Example:
;; (format-port #x0aff)
;; => "10,255"

(define (format-port port)
  (string-append
   (number->string (bitwise-and (arithmetic-shift port -8) 255))
   ","
   (number->string (bitwise-and port 255))))

