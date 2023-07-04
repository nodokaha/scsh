;; converts an ip-string to an 32bit integer internet-address
(define (address32->ip-string ip)
  (octet-ip->ip-string (address32->octet-ip ip)))
  
;; converts an ip-string to an 32bit integer internet-address
(define (ip-string->address32 ip)
  (octet-ip->address32 (ip-string->octet-ip ip)))

;; checks if a string is a ip
(define (ip-string? s)
  (define (byte-as-string? string) 
    (let ((number (string->number string)))
      (and number
	   (>= number 0)
	   (< number 256))))
  (cond
   ((regexp-search ip-string-regexp  s)
    => (lambda (match)
	 (and (byte-as-string? (match:substring match 1))
	      (byte-as-string? (match:substring match 2))
	      (byte-as-string? (match:substring match 3))
	      (byte-as-string? (match:substring match 4)))))
   (else #f)))

;; converts an ip-string to octets
(define (ip-string->octet-ip s)
  (cond
   ((regexp-search ip-string-regexp  s)
    => (lambda (match)
	 (list
	  (ascii->char (string->number (match:substring match 1)))
	  (ascii->char (string->number (match:substring match 2)))
	  (ascii->char (string->number (match:substring match 3)))
	  (ascii->char (string->number (match:substring match 4))))))
   (else
    (error "invalid ip-string" s))))

;; converts an octeted-ip to a 32bit integer internet-address
(define (octet-ip->address32 ip)
  (+ (arithmetic-shift (char->ascii (list-ref ip 0)) 24)
     (arithmetic-shift (char->ascii (list-ref ip 1)) 16)
     (arithmetic-shift (char->ascii (list-ref ip 2)) 8)
     (char->ascii (list-ref ip 3))))

;; converts a 32 bit integer internet-address to an octeted-ip
(define (address32->octet-ip ip)
  (list (ascii->char (arithmetic-shift ip -24))
	(ascii->char (modulo (arithmetic-shift ip -16) 256))
	(ascii->char (modulo (arithmetic-shift ip -8) 256))
	(ascii->char (modulo ip 256))))


;; converts an octeted-ip to an human readable ip-string
(define (octet-ip->ip-string s)
  (format #f
	  "~a.~a.~a.~a" 
	  (char->ascii (list-ref s 0))
	  (char->ascii (list-ref s 1))
	  (char->ascii (list-ref s 2))
	  (char->ascii (list-ref s 3))))

;; returns a in-addr.arpa name-string or #f (needed to resolve hostname by ip)
(define (ip-string->in-addr.arpa-string s)
  (cond
   ((regexp-search ip-string-regexp s)
    => (lambda (match)
	 (string-append
	  (match:substring match 4) "."
	  (match:substring match 3) "."
	  (match:substring match 2) "."
	  (match:substring match 1) "."
	  "in-addr.arpa")))
   (else #f)))

(define ip-string-regexp (rx (: bos 
			  (submatch (** 1 3 digit)) "." 
			  (submatch (** 1 3 digit)) "." 
			  (submatch (** 1 3 digit)) "." 
			  (submatch (** 1 3 digit)) 
			  eos)))
