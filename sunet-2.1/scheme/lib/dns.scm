; 
; dns.scm
;
; Implementation of the RFC1035 

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2002 by Marcus Crestani.
;;; Copyright (c) 2002-2003 by Martin Gasbichler
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

; domain names - implementation and specification
; based on the PLT-implementation.
; 
;
; TODO: 
;       - test, test, test
;       - types from newer RFCs (41, unknown)
;       - more documentation
;
; ---
; sample usage & documentation:
;
;  <ip-address32> is a 32bit integer internet->address, shortly address32.
;  <ip-string> is a string in standard dot notation "xxx.xxx.xxx.xxx".
;  <name> is a string
;
;  <nameserver> can either be a domainname, an ip-string or an ip-address32.
;  if it is a domainname, its ip is looked up on a nameserver listed in
;  /etc/resolv.conf.
;
;  (dns-find-nameserver-list) --> <ip-string list>
;  this parses the /etc/resolv.conf file and returns the found 
;  nameserver in a list of dotted strings.
;
;  (dns-find-nameserver) --> <ip-string>
;  this parses the /etc/resolv.conf file and returns the first found 
;  nameserver in dotted string notation.
;
;  (dns-check-namservers [nameserver list]) --> undefined
;  checks if the given nameservers are reachable. If no argument is given,
;  the nameservers in /etc/resolv.conf are checked.
;
;  
;  (dns-lookup-name <name> [nameserver list] [use-cache?]) --> <ip-address32>
;  (dns-lookup-ip <ip-string | ip-address32> [nameserver list] [use-cache?]) 
;                     --> <name>
;  (dns-lookup-nameserver <name> [nameserver list] [use-cache?]) 
;                     --> <list of ip-address32s of authoritative nameservers>
;  (dns-lookup-mail-exchanger <name> [nameserver list] [use-cache?]) 
;                     --> <list of names of mail-exchangers>
; 
;  dns-lookup-name, dns-lookup-ip, dns-lookup-nameserver and 
;  dns-lookup-mail-exchanger are "simple lookup functions",
;  they return the wanted information or #f.
;  dns-lookup-ip can either be given an ip-string or an ip-address32.
;   
;  concurrent dns lookup:
;  if a list of nameservers is given to the optional <nameserver> argument,
;  a concurrent lookup to all nameservers in this list is started.
;  The nameservers in this list could either be ip-strings or ip-address32s.
;  example: (dns-lookup-name "www.uni-tuebingen.de" (dns-find-nameserver-list))
;           starts an concurrent lookup which contacts all nameservers in 
;           /etc/resolv.conf.
;
;
;  (dns-lookup <name | ip-string | ip-address32> <type> [nameserver list]) 
;                     --> <dns-message>
;  (show-dns-message <dns-message) --> the whole message, human readable
;
;  a <dns-message> is a record, with several entries, which holds the whole
;  query/response dialog. the simplest way to get detailed information about
;  the record structure is to view the result of show-dns-message.
;
;  dns-lookup returns much more information than the simple lookup functions, 
;  only useful in very special cases.
;  
;
;  some lookups return a hostname (e.g. mx). 
;  many applications need instead of a hostname a ip address.
;  force-ip and force-ip-list guarantee that a ip address is
;  returned.
;
;  (force-ip <name>) --> <ip>
;  (force-ip-list <list of names>) --> <list of ips>
;
;
;  useful converters:
;
;  (address32->ip-string <ip-address32>) -> <ip-string>
;  (ip-string->address32 <ip-string>) -> <ip-address32>



;; --- error conditions

;; supertype of all errors signaled by this library
(define-condition-type 'dns-error '(error))
(define dns-error? (condition-predicate 'dns-error))

(define-condition-type 'parse-error '(dns-error))
(define parse-error? (condition-predicate 'parse))

(define-condition-type 'unexpected-eof-from-server '(dns-error))
(define unexpected-eof-from-server? (condition-predicate 'unexpected-eof-from-server))

(define-condition-type 'bad-address '(dns-error))
(define bad-address? (condition-predicate 'bad-address))

(define-condition-type 'no-nameservers '(dns-error))
(define no-nameservers? (condition-predicate 'no-nameservers))

(define-condition-type 'no-nameserver-given '(dns-error))
(define no-nameserver-given? (condition-predicate 'no-nameserver-given))

(define-condition-type 'bad-nameserver '(dns-error))
(define bad-nameserver? (condition-predicate 'bad-nameserver))

(define-condition-type 'not-a-hostname '(dns-error))
(define not-a-hostname? (condition-predicate 'not-a-hostname))

(define-condition-type 'not-a-ip '(dns-error))
(define not-a-ip? (condition-predicate 'not-a-ip))

;; supertype of all errors signaled if the dns server returned a non-sero
;; reply code
(define-condition-type 'dns-server-error '(dns-error))
(define dns-server-error? (condition-predicate 'dns-server-error))

(define-condition-type 'dns-format-error '(dns-server-error))
(define dns-format-error? (condition-predicate 'dns-format-error))

(define-condition-type 'dns-server-failure '(dns-server-error))
(define dns-server-failure? (condition-predicate 'dns-server-failure))

(define-condition-type 'dns-name-error '(dns-server-error))
(define dns-name-error? (condition-predicate 'dns-name-error))

(define-condition-type 'dns-not-implemented '(dns-server-error))
(define dns-not-implemented? (condition-predicate 'dns-not-implemented))

(define-condition-type 'dns-refused '(dns-server-error))
(define dns-refused? (condition-predicate 'dns-refused))

(define (dns-error condition . stuff)
  (apply signal condition (dns-error->string condition) stuff))

(define (dns-error->string condition)
  (string-append 
   "dns-error: "
   (case condition
     ((parse-error)
      "parse: error parsing server message")
     ((unexpected-eof-from-server)
      "send-receive-message: unexpected EOF from server")
     ((bad-address)
      "dns-get-information: bad address (in combination with query type)")
     ((no-nameservers)
      "dns-find-nameserver: no nameservers found in /etc/resolv.conf")
     ((no-nameserver-given)
      "dns-find-nameserver: no nameservers given")
     ((bad-nameserver)
      "send-receive-message: could not establish connection to server (no valid nameserver given)")
     ((not-a-hostname)
      "no hostname given")
     ((not-a-ip)
      "no ip given")
     ((dns-format-error) 
      "error from server: (1) format error")
     ((dns-server-failure) 
      "error from server: (2) server failure")
     ((dns-name-error) 
      "error from server: (3) name error")
     ((dns-not-implemented) 
      "error from server: (4) not implemented")
     ((dns-refused) 
      "error from server: (5) refused")
     (else (error "Unknown dns-error" condition)))))
  

;;; -- globals and types
;; off
(define *nul* (ascii->char 0))

;; on
(define *on* (ascii->char 1))

;; message types
(define-enumerated-type message-type :message-type
  message-type?
  the-message-types
  message-type-name
  message-type-number
  (unknown				; types, which are not yet implemented
   a					; a host address 
   ns					; an authoritative name server
   md					; (obsolete)
   mf					; (obsolete)
   cname				; the canonical name for an alias
   soa					; marks the start of a zone of authority
   mb					; (experimental)
   mg					; (experimental)
   mr					; (experimental)
   null					; (experimental)
   wks					; a well known service description
   ptr					; a domain name pointer
   hinfo				; host information
   minfo				; (experimental)
   mx					; mail exchange
   txt))				; text strings

;; message classes
(define-enumerated-type message-class :message-class
  message-class?
  the-message-classes
  message-class-name
  message-class-number
  (placeholder ; this starts at 0...
   in      ; the Internet
   cs      ; (obsolete)
   ch      ; the CHAOS class
   hs))    ; Hesoid


;;; -- useful stuff

;; number: 0<= x < 256
;; octet-pair: (char char)
;; octet-quad: (char char char char)
;; name: string *{"." string}
;; octets: *{(char *char)} nullchar
;; octet-ip: (char char char char)
;; address32: 0 <= x < 2^32-1
;; ip-string: "www.xxx.yyy.zzz"
;; ip-string-arpa: "zzz.yyy.xxx.www.in-addr.arpa"

;; encodes numbers (16bit) to octets
(define (number->octet-pair n)
  (list (ascii->char (arithmetic-shift n -8))
	(ascii->char (modulo n 256))))

;; decodes octets to numbers (16bit)
(define (octet-pair->number a b)
  (+ (arithmetic-shift (char->ascii a) 8)
     (char->ascii b)))

;; encodes numbers (32bit) to octets, needed for ttl
(define (number->octet-quad n)
  (list (ascii->char (arithmetic-shift n -24))
	(ascii->char (modulo (arithmetic-shift n -16) 256))
	(ascii->char (modulo (arithmetic-shift n -8) 256))
	(ascii->char (modulo n 256))))

;; decodes octets to numbers, needed for 32bit ttl
(define (octet-quad->number a b c d)
  (+ (arithmetic-shift (char->ascii a) 24)
     (arithmetic-shift (char->ascii b) 16)
     (arithmetic-shift (char->ascii c) 8)
     (char->ascii d)))

;; encodes a domain-name string to octets
(define (name->octets s)
  (define (encode-portion s)
    (cons
     (ascii->char (string-length s))
     (string->list s)))

  (let loop ((s s))
    (cond
     ((regexp-search (rx (: bos (submatch (* (~ "."))) "." (submatch (* any))))
		     s)
      => (lambda (match)
	   (append
	    (encode-portion (match:substring match 1))
	    (loop (match:substring match 2)))))
     (else
      (if (= 0 (string-length s))
	  (list *nul*)
	  ;;; TODO isn't this case an error?
	  (append
	   (encode-portion s)
	   (list *nul*)))))))


;; for tcp: message has to be tagged with its length
(define (add-size-tag m)
  (append (number->octet-pair (length m)) m))




;; calculates a "random" number, needed for message-ids
;; TODO use SRFI-27
(define random
  (let ((crank (make-random (modulo (time) (- (expt 2 27) 1)))))
    (lambda (limit)
      (quotient (* (modulo (crank) 314159265)
		   limit)
		314159265))))

;; checks if v is a address32
(define (address32? v)
  (and (number? v)
       (<= 0 v #xffffffff)))
      
;; filters types in a list of rrs 
(define (filter-type list type)
  (filter (lambda (rr) 
	    (eq? (resource-record-type rr) type))
	  list))

;; sorts a mx-resource-record-list by preference. needed for dns-lookup-mail-exchanger.
(define (sort-by-preference mx-list)
  (sort-list mx-list 
	     (lambda (a b) 
	       (< (resource-record-data-mx-preference (resource-record-data a)) (resource-record-data-mx-preference (resource-record-data b))))))


;; returns a IP if available (additonal type-a processing)
(define (force-ip name)
  (let loop ((result (dns-lookup-name name)))
    (if (ip-string? result)
	result
	(loop (dns-lookup-name result)))))

;; returns a list of IPs (additional type-a processing)
(define (force-ip-list names)
  (map (lambda (elem) (force-ip elem)) names))


;; a standard query header, usefull for most queries
(define (make-std-query-header id question-count)
  (let* ((qr 'query)        ; querytype: query 0, response 1
	 (opcode 0)    ; opcode: query 0, iquery 1 (OBSOLETE), status 2
	 (aa #f)        ; authorative answer (in answers only)
	 (tc #f)        ; truncation (size matters only with UDP)
	 (rd #t)        ; recursion desired: nameserver pursues the query recursivly (optional)
	 (ra #f)        ; recursion available (in answers only)
	 (zero 0)         ; future use
         (response-code 0)     ; response code: error conditions (in answers only)
	 (question-count question-count)
	 (answer-count 0)   ; answer count (in answers only)
	 (nameserver-count 0)   ; name server resources (in answers only)
	 (additional-count 0))  ; additional records (in answers only)
    
    (make-header 
     id 
     (make-flags qr opcode aa tc rd ra zero response-code)
     question-count answer-count nameserver-count additional-count)))


;; makes a query-message (header and question only)
;; TODO does this really work for several questions as well?
(define (make-query-message header question . questions)
  (let ((questions (cons question questions)))
    (make-message header questions '() '() '()
		  (apply 
		   append
		   (header->octets header)
		   (map question->octets questions)))))

(define (make-simple-query-message name type class)
  (make-query-message (make-std-query-header (random 256) 1)
		      (make-question name type class)))

;; makes a resource record for ans, nss, ars (name, type, class, ttl, data)
(define (make-octet-rr name type class ttl rdata)
  (let* ((name (name->octets name))
	 (type (number->octet-pair (message-type-number type)))
	 (class (number->octet-pair (message-class-number class)))
	 (ttl (number->octet-quad ttl))
	 (rdlength (number->octet-pair (length rdata)))
	 (rdata rdata))
    (append name type class ttl rdlength rdata)))



;;; -- parsed message records

;;; -- dns-message: complete data-structure of an dns-lookup
(define-record-type dns-message :dns-message
  (make-dns-message query reply cache? protocol tried-nameservers)
  dns-message?
  (query dns-message-query)
  (reply dns-message-reply)
  (cache? dns-message-cache?)
  (protocol dns-message-protocol)
  (tried-nameservers dns-message-tried-nameservers))

;; message
(define-record-type message :message
  (make-message header questions answers nameservers additionals source)
  message?
  (header message-header)
  (questions message-questions)
  (answers message-answers)
  (nameservers message-nameservers)
  (additionals message-additionals)
  (source message-source))

;; header
(define-record-type header :header
  (make-header id flags question-count answer-count nameserver-count 
	       additional-count)
  header?
  (id header-id)
  (flags header-flags)
  (question-count header-question-count)
  (answer-count header-answer-count)
  (nameserver-count header-nameserver-count)
  (additional-count header-additional-count))

;;; -- message constructors: encode to octet-messages

;; makes an message header
(define (header->octets header)
  (let* ((header-id (number->octet-pair (header-id header)))
	 (header-question-count (number->octet-pair (header-question-count header)))
	 (header-answer-count (number->octet-pair (header-answer-count header)))
	 (header-nameserver-count (number->octet-pair 
				   (header-nameserver-count header)))
	 (header-additional-count (number->octet-pair 
				   (header-additional-count header))))
    (append header-id
	    (flags->octets (header-flags header))
	    header-question-count
	    header-answer-count
	    header-nameserver-count
	    header-additional-count)))

;; flags
(define-record-type flags :flags
  (make-flags query-type opcode authoritative? truncated? recursion-desired? 
	      recursion-available? zero response-code)
  flags?
  (query-type            flags-query-type)
  (opcode                flags-opcode)
  (authoritative?        flags-authoritative?)
  (truncated?            flags-truncated?)
  (recursion-desired?    flags-recursion-desired?)
  (recursion-available?  flags-recursion-available?)
  (zero                  flags-zero)
  (response-code         flags-response-code))

(define (make-flags-from-numbers
	 querytype opcode authoritative? truncated? recursion-desired? recursion-available? 
	 zero response-code)
  (make-flags
   (if (zero? querytype) 'query 'response)
   opcode
   (not (zero? authoritative?))
   (not (zero? truncated?))
   (not (zero? recursion-desired?))
   (not (zero? recursion-available?))
   zero
   (case response-code
     ((0) 'dns-no-error)
     ((1) 'dns-format-error)
     ((2) 'dns-server-failure)
     ((3) 'dns-name-error)
     ((4) 'dns-not-implemented)
     ((5) 'dns-refused))))
   
(define (flags->octets flags)
  (define (boolean->0/1 bool)
    (if bool 1 0))
  (list 
   (ascii->char (+ (arithmetic-shift 
		    (if (eq? (flags-query-type flags) 'query) 0 1) 7)
		   (arithmetic-shift (flags-opcode flags) 3)
		   (arithmetic-shift 
		    (boolean->0/1 (flags-authoritative? flags)) 2)
		   (arithmetic-shift 
		    (boolean->0/1 (flags-truncated? flags)) 1)
		   (boolean->0/1 (flags-recursion-desired? flags))))
   (ascii->char (+ (arithmetic-shift 
		    (boolean->0/1 (flags-recursion-available? flags)) 7)
		   (arithmetic-shift (flags-zero flags) 4)
		   (flags-response-code flags)))))


;; question
(define-record-type question :question
  (make-question name type class)
  question?
  (name question-name)
  (type question-type)
  (class question-class))

;; makes a question (name, type, class)
(define (question->octets q)
  (let* ((qname   (name->octets (question-name q)))
	 (qtype   (number->octet-pair 
		   (message-type-number (question-type q))))
	 (qclass  (number->octet-pair 
		   (message-class-number (question-class q)))))
    (append qname qtype qclass)))

;;type  resource-record
(define-record-type resource-record :resource-record
  (make-resource-record name type class ttl data)
  resource-record?
  (name  resource-record-name)
  (type  resource-record-type)
  (class resource-record-class)
  (ttl   resource-record-ttl)
  (data  resource-record-data))

;; cache
(define-record-type cache :cache
  (make-cache answer ttl time)
  cache?
  (answer cache-answer)
  (ttl cache-ttl)
  (time cache-time))

;;; -- message parser

;; parses a domain-name in an message. returns the name and the rest of the message.
(define (parse-name start message)
  (let ((v (char->ascii (car start))))
    (cond
     ((zero? v)
      ;; End of name
      (values #f (cdr start)))
     ((zero? (bitwise-and #xc0 v))
      ;; Normal label
      (let loop ((len v)
		 (start (cdr start))
		 (accum '()))
	(cond
	 ((zero? len)
	  (call-with-values
	   (lambda () (parse-name start message))
	   (lambda (s start)
	     (let ((s0 (list->string (reverse! accum))))
	       (values (if s
			   (string-append s0 "." s)
			   s0)
		       start)))))
	 (else (loop (- len 1)
		     (cdr start)
		     (cons (car start) accum))))))
     (else
      ;; Compression offset
      (let ((offset (+ (arithmetic-shift (bitwise-and #x3f v) 8)
		       (char->ascii (cadr start)))))
	(call-with-values
	 (lambda () (parse-name (list-tail message offset) message))
	 (lambda (s ignore-start)
	   (values s (cddr start)))))))))

;; parses a question in a message. returns the question and the rest of the message.
(define (parse-question start message)
  (call-with-values
   (lambda () (parse-name start message))
   (lambda (name start)
     (let ((type (vector-ref the-message-types 
			     (octet-pair->number (car start) (cadr start))))
	   (start (cddr start)))
       (let ((class (vector-ref the-message-classes 
				(octet-pair->number (car start) (cadr start))))
	     (start (cddr start)))
	 (values (make-question name type class) start))))))

;; parses a resourcerecord in a message. returns the rr and the rest of the message.

(define (type-number->type type-number)
  (if (>= type-number (vector-length the-message-types))
      'unsupported-message-type
      (vector-ref the-message-types type-number)))

(define (class-number->class class-number)
  (if (>= class-number (vector-length the-message-classes))
      'unsupported-message-class
      (vector-ref the-message-classes class-number)))

(define (parse-rr start message)
  (call-with-values
   (lambda () (parse-name start message))
   (lambda (name start)
     (let ((type  (type-number->type
                   (octet-pair->number (car start) (cadr start))))
	   (start (cddr start)))
       (let ((class (class-number->class
                     (octet-pair->number (car start) (cadr start))))
	     (start (cddr start)))
	 (let ((ttl (octet-quad->number (car start) (cadr start)
					(caddr start) (cadddr start)))
	       (start (cddddr start)))
	   (let ((len (octet-pair->number (car start) (cadr start)))
		 (start (cddr start)))
	     ;; Extract next len bytes of data:
	     (let loop ((len len)
			(start start)
			(accum '()))
	       (if (zero? len)
		   (values (make-resource-record name type class ttl (parse-resource-record-data type class (reverse! accum) message)) start)
		   (loop (- len 1)
			 (cdr start)
			 (cons (car start) accum)))))))))))

;;; -- resource-record-data-type records

(define-record-type resource-record-data-a :resource-record-data-a
  (make-resource-record-data-a ip)
  resource-record-data-a?
  (ip resource-record-data-a-ip))

(define-record-type resource-record-data-ns :resource-record-data-ns
  (make-resource-record-data-ns name)
  resource-record-data-ns?
  (name resource-record-data-ns-name))

(define-record-type resource-record-data-cname :resource-record-data-cname
  (make-resource-record-data-cname name)
  resource-record-data-cname?
  (name resource-record-data-cname-name))

;; ###
;; hinfo not correctly implemented, trying to find examples 
(define-record-type resource-record-data-hinfo :resource-record-data-hinfo
  (make-resource-record-data-hinfo data)
  resource-record-data-hinfo?
  (data resource-record-data-hinfo-data))

(define-record-type resource-record-data-mx :resource-record-data-mx
  (make-resource-record-data-mx preference exchanger)
  resource-record-data-mx?
  (preference resource-record-data-mx-preference)
  (exchanger resource-record-data-mx-exchanger))

(define-record-type resource-record-data-ptr :resource-record-data-ptr
  (make-resource-record-data-ptr name)
  resource-record-data-ptr?
  (name resource-record-data-ptr-name))

(define-record-type resource-record-data-soa :resource-record-data-soa
  (make-resource-record-data-soa mname rname serial refresh retry expire minimum)
  resource-record-data-soa?
  (mname resource-record-data-soa-mname)
  (rname resource-record-data-soa-rname)
  (serial resource-record-data-soa-serial)
  (refresh resource-record-data-soa-refresh)
  (retry resource-record-data-soa-retry)
  (expire resource-record-data-soa-expire)
  (minimum resource-record-data-soa-minimum))

;; ### same as hinfo
(define-record-type resource-record-data-txt :resource-record-data-txt
  (make-resource-record-data-txt text)
  resource-record-data-txt?
  (text resource-record-data-txt-text))

;; ### same as hinfo and txt
(define-record-type resource-record-data-wks :resource-record-data-wks
  (make-resource-record-data-wks data)
  resource-record-data-wks?
  (data resource-record-data-wks-data))

;;

(define (parse-resource-record-data type class data message)
  (cond
   ((eq? type (message-type a))
    (make-resource-record-data-a (octet-ip->address32 data)))
   
   ((eq? type (message-type ns))
    (make-resource-record-data-ns (call-with-values 
		      (lambda () (parse-name data message))
		      (lambda (name rest) name))))

   ((eq? type (message-type cname))
    (make-resource-record-data-cname (call-with-values
			 (lambda () (parse-name data message))
			 (lambda (name rest) name))))

   ((eq? type (message-type mx))
    (make-resource-record-data-mx (octet-pair->number (car data) (cadr data))
		     (call-with-values
		      (lambda ()(parse-name (cddr data) message))
		      (lambda (name rest) name))))

   ((eq? type (message-type ptr))
    (make-resource-record-data-ptr (call-with-values
		       (lambda () (parse-name data message))
		       (lambda (name rest) name))))
   
   ((eq? type (message-type soa))
    (call-with-values
     (lambda () (parse-name data message))
     (lambda (mname rest)
       (call-with-values
	(lambda () (parse-name rest message))
	(lambda (rname rest)
	  (let ((serial (octet-quad->number (car rest) (cadr rest) (caddr rest) (cadddr rest)))
		(rest (cddddr rest)))
	    (let ((refresh (octet-quad->number (car rest) (cadr rest) (caddr rest) (cadddr rest)))
		  (rest (cddddr rest)))
	      (let ((retry (octet-quad->number (car rest) (cadr rest) (caddr rest) (cadddr rest)))
		    (rest (cddddr rest)))
		(let ((expire (octet-quad->number (car rest) (cadr rest) (caddr rest) (cadddr rest)))
		      (rest (cddddr rest)))
		  (let ((minimum (octet-quad->number (car rest) (cadr rest) (caddr rest) (cadddr rest)))
			(rest (cddddr rest)))
		    (make-resource-record-data-soa mname rname serial refresh retry expire minimum)))))))))))

   ((eq? type (message-type hinfo))
    (make-resource-record-data-hinfo (list->string data)))

   ((eq? type (message-type txt))
    (make-resource-record-data-txt (list->string data)))
   
   ((eq? type (message-type wks))
    (make-resource-record-data-wks data))

   (else (list data))))

;; parses n-times a message with parse. returns a list of parse-returns.
(define (parse-n parse start message n)
  (let loop ((n n) (start start) (accum '()))
    (if (zero? n)
	(values (reverse! accum) start)
	(call-with-values
	 (lambda () (parse start message))
	 (lambda (rr start)
	   (loop (- n 1) start (cons rr accum)))))))

;; parses a message-headers flags. returns the flags.
(define (parse-flags message)
  (let ((v0 (list-ref message 2))
	(v1 (list-ref message 3)))
    ;; Check for error code:
    (let ((response-code  (bitwise-and #xf (char->ascii v1)))
	  (zero      (arithmetic-shift (bitwise-and 112 (char->ascii v1)) -4))
	  (ra     (arithmetic-shift (bitwise-and 64 (char->ascii v1)) -7))
	  (rd     (bitwise-and 1 (char->ascii v0)))
	  (tc     (arithmetic-shift (bitwise-and 2 (char->ascii v0)) -1))
	  (aa     (arithmetic-shift (bitwise-and 4 (char->ascii v0)) -2))
	  (opcode (arithmetic-shift (bitwise-and 120 (char->ascii v0)) -3))
	  (qr     (arithmetic-shift (bitwise-and 128 (char->ascii v0)) -7))) 
      (make-flags-from-numbers qr opcode aa tc rd ra zero response-code))))


;; parses a message-header. returns the header.
(define (parse-header message)
  (let ((id       (octet-pair->number (list-ref message 0) (list-ref message 1)))
	(flags    (parse-flags message))
	(question-count (octet-pair->number (list-ref message 4) (list-ref message 5)))
	(an-count (octet-pair->number (list-ref message 6) (list-ref message 7)))
	(ns-count (octet-pair->number (list-ref message 8) (list-ref message 9)))
	(ar-count (octet-pair->number (list-ref message 10) (list-ref message 11))))
    (make-header id flags question-count an-count ns-count ar-count)))


;; parses a message. returns the parsed message.
(define (parse message)
  (let* ((header (parse-header message))
	 (start (list-tail message 12)))
    (call-with-values
     (lambda () (parse-n parse-question start message (header-question-count header)))
     (lambda (qds start)
       (call-with-values
	(lambda () (parse-n parse-rr start message (header-answer-count header)))
	(lambda (ans start)
	  (call-with-values
	   (lambda () (parse-n parse-rr start message (header-nameserver-count header)))
	   (lambda (nss start)
	     (call-with-values
	      (lambda () (parse-n parse-rr start message (header-additional-count header)))
	      (lambda (ars start)
		(if (not (null? start))
		    (dns-error 'parse-error))
		(make-message header qds ans nss ars message)))))))))))



;;; -- send, receive and validate message

;; checks if the received reply is valid. returns #t or error-msg.
(define (reply-acceptable? reply query)
  ;; Check correct id
  (if (not (= (header-id (message-header reply))
	      (header-id (message-header query))))
      ;; TODO replace error
      (error "send-receive-message: bad reply id from server"))
  ;; Check for error code:
  (let ((response-code (flags-response-code 
			(header-flags (message-header reply)))))
    (if (not (eq? response-code 'dns-no-error))
	(dns-error response-code))))


(define *max-tries* 3)
(define *timeout* 1)

;; connects to nameserver and sends and receives messages. returns the reply.
;; here: via TCP
(define (send-receive-message-tcp nameservers query)
  (receive (reply hit-ns other-nss)
	   (let ((sockets (map
			   (lambda (nameserver)
			     (let ((sock (create-socket protocol-family/internet
							socket-type/stream))
				   (addr (internet-address->socket-address
					  nameserver 53)))
			       ;; we ignore the return value and select
			       ;; unconditionally later
			       (call-with-current-continuation
				(lambda (k)
				  (with-handler (lambda (cond more)
						  (k #f))
						(lambda ()
						  (connect-socket-no-wait sock addr)
						  sock))))))
			   nameservers)))
	(let* ((nameservers
		(let loop ((sockets sockets)
			   (nss nameservers))
		  (cond 
		   ((or (null? sockets) (null? nss)) '())
		   ((socket? (car sockets))
		    (cons (car nss) (loop (cdr sockets) (cdr nss))))
		   (else (loop (cdr sockets) (cdr nss))))))

	       (sockets (filter socket? sockets))
	       (ws (map socket:outport sockets))
	       (wport-nameserver-alist (map cons ws nameservers))
	       (wport-socket-alist (map cons ws sockets)))
	  (dynamic-wind
	   (lambda () 
	     'nothing-to-be-done-before)
	   (lambda ()
	     (let loop-port-channels ((tried-channels '())
				      (number-tries 1))
	       (letrec ((delete-list
			 (lambda (elems list)
			   (cond
			    ((null? elems) list)
			    ((null? list) '())
			    (else (delete-list (cdr elems) (delete (car elems) list))))))
			(ws-new (delete-list tried-channels ws)))
		 (if (or (null? ws-new) (>= number-tries *max-tries*))
		     (dns-error 'bad-nameserver)
		     (let ((ready (apply select-port-channels *timeout* ws)))
		       (if (= (length tried-channels) (length ws))
			   (dns-error 'bad-nameserver)
			   (let loop-ready-channels ((ready-channels ready))
			     (if (null? ready-channels)
				 (loop-port-channels (append tried-channels ready) (+ number-tries 1))
				 (let* ((w (car ready-channels))
					(hit-ns (cdr (assoc w wport-nameserver-alist)))
					(sock (cdr (assoc w wport-socket-alist))))
				   (if (not (connect-socket-successful? sock))
				       (loop-ready-channels (cdr ready-channels))
				       (let ((query-string  
					      (list->string (add-size-tag (message-source query))))
					     (r (socket:inport sock)))
					 (call-with-current-continuation
					  (lambda (k)
					    (with-handler (lambda (cond more)
							    (k (loop-ready-channels (cdr ready-channels))))
							  (lambda ()
							    (display query-string w)
							    (force-output w)
							    (let ((a (read-char r))
								  (b (read-char r)))
							      (let ((len (octet-pair->number a b)))
								(let ((s (read-string len r)))
								  (if (and (not (= 0 (string-length s))) 
									   (not (= len (string-length s))))
								      (dns-error 'unexpected-eof-from-server))
								  (values (parse (string->list s))
									  hit-ns
									  (delete hit-ns nameservers))))))))))))))))))))
	   (lambda ()
	     (for-each close-socket sockets)))))
    (reply-acceptable? reply query)
    (values reply
	    hit-ns
	    other-nss)))

;; here: via UDP
(define (send-receive-message-udp nameservers query)
  (receive (reply hit-ns other-nss)
      (let ((sockets (map (lambda (nameserver)
			    (let ((sock (create-socket protocol-family/internet
						       socket-type/datagram))
				  (addr (internet-address->socket-address
					 nameserver 53)))
			      (connect-socket sock addr)
			      sock))
			  nameservers)))
	(let ((rs (map socket:inport sockets))
	      (ws (map socket:outport sockets)))
	  (dynamic-wind
	   (lambda ()
	     'nothing-to-be-done-before)
	   (lambda ()
	     (let ((query-string (list->string (message-source query)))
		   (rsv (list->vector rs))
		   (rport-nameserver-alist (map cons rs nameservers))
		   (rport-socket-alist (map cons rs sockets)))
	       (for-each (lambda (w) (display query-string w)) ws)
	       (for-each force-output ws)
	       (let loop-port-channels ((tried-channels '())
					(number-tries 1))
		 (letrec ((delete-list
			   (lambda (elems list)
			     (cond
			      ((null? elems) list)
			      ((null? list) '())
			      (else (delete-list (cdr elems) (delete (car elems) list))))))
			  (rs-new (delete-list tried-channels rs)))
		   (if (or (null? rs-new) (>= number-tries *max-tries*))
		       (dns-error 'bad-nameserver)
		       (let ((ready (apply select-port-channels *timeout* rs-new)))
			 (if (= (length tried-channels) (length rs))
			     (dns-error 'bad-nameserver)
			     (let loop-ready-channels ((ready-channels ready))
			       (if (null? ready-channels)
				   (loop-port-channels (append tried-channels ready) (+ number-tries 1))
				   (let* ((r (car ready-channels))
					  (hit-ns (cdr (assoc r rport-nameserver-alist))))
				     (if (not (connect-socket-successful? (cdr (assoc r rport-socket-alist))))
					 (loop-ready-channels (cdr ready-channels))
					 ;; 512 is the maximum udp-message size:
					 (let ((answer (string->list (read-string/partial 512 r))))
					   (if (null? answer)
					       (loop-ready-channels (cdr ready-channels))
					       (values (parse answer)
						       hit-ns
						       (delete hit-ns nameservers)))))))))))))))
	   (lambda ()
	     (for-each close-socket sockets)))))
      (reply-acceptable? reply query)
      (if (flags-truncated? (header-flags (message-header reply)))
	  (send-receive-message-tcp nameservers query)
	  (values reply
		  hit-ns
		  other-nss))))


;;; -- cache

;; creates the cache, an empty string-table
(define cache (make-string-table))

;; resets the cache
(define (dns-clear-cache!)
  (set! cache (make-string-table)))

;; searches in a dns-msg for the shortest ttl. this is needed for cache-management.
(define (find-shortest-ttl dns-msg)
  (letrec ((minimum #f)
	   (find-shortest-ttl-1
	    (lambda (dns-msg)
	      (cond
	       ((dns-message? dns-msg) 
		(find-shortest-ttl-1 (dns-message-reply dns-msg)))
	       ((message? dns-msg)
		(for-each (lambda (x) (find-shortest-ttl-1 x)) (message-answers dns-msg))
		(for-each (lambda (x) (find-shortest-ttl-1 x)) (message-nameservers dns-msg))
		(for-each (lambda (x) (find-shortest-ttl-1 x)) (message-additionals dns-msg))
		minimum)
	       ((resource-record? dns-msg)
		(cond
		 ((not minimum)  (set! minimum (resource-record-ttl dns-msg)))
		 (else
		  (if (and (not minimum) (> minimum (resource-record-ttl dns-msg)))
		      (set! minimum (resource-record-ttl dns-msg))))))))))
    (find-shortest-ttl-1 dns-msg)))


(define (make-key qds nameserver)
  (let*;; cache-key relevant data
      ((name (question-name (car qds)))
       (type (question-type (car qds)))
       (class (question-class (car qds))))
    (format #f "~a;~a;~a;~a" 
	    nameserver 
	    name 
	    (message-type-name type)
	    (message-class-name class))))

(define (lookup-cache qds nameserver)
  (let* ((key (make-key qds nameserver))
	 (found-data (table-ref cache key)))
    (cond
     ((and found-data
	   ;; checks if cached-data is still valid
	   (< (time) (+ (cache-time found-data) (cache-ttl found-data))))
      found-data)
     (else #f))))

(define (update-cache! key entry)
  (table-set! cache key entry))

(define (dns-query-no-cache query protocol nameservers tried)
	 ;; returns new retrieved data
	 (receive (dns-msg hit-ns nss-with-no-reply)
	     (send-receive-message nameservers query protocol)
	   (values 
	    (make-dns-message query dns-msg  #f protocol (reverse tried))
	    hit-ns
	    nss-with-no-reply)))

(define (dns-query-with-cache query protocol nameservers tried)
  (let ((qds (message-questions query)))
    (let lp ((ns nameservers))
      (if (null? ns)
	  (receive (reply-msg hit-ns nss-with-no-reply)
	      (send-receive-message nameservers query protocol)
	    (update-cache! (make-key qds hit-ns) 
			   (make-cache reply-msg (find-shortest-ttl reply-msg) (time)))
	    ;; returns new retrieved data and updates cache
	    (values (make-dns-message query reply-msg #f protocol (reverse tried))
		    hit-ns
		    nss-with-no-reply))
	  (cond ((lookup-cache qds (car ns))
		 => (lambda (found-data)
		      ;; returns cached data
		      (values (make-dns-message query (cache-answer found-data) #t protocol '())
			      #f
			      nameservers)))
		(else (lp (cdr ns))))))))

(define-enumerated-type network-protocol :network-protocol
  network-protocol?
  network-protocols
  network-protocol-name
  network-protocol-index
  (udp tcp))

(define (send-receive-message nameservers query protocol)
  ((cond 
    ((eq? protocol (network-protocol tcp)) send-receive-message-tcp)
    ((eq? protocol (network-protocol udp)) send-receive-message-udp))
   nameservers query))
  
;; makes a dns-query. optional cache-check. 
;; returns a dns-message with cache-flag and either cache-data or new received data.
(define (dns-query/cache query use-cache? protocol nameservers tried)
      (if use-cache?
	  (dns-query-with-cache query protocol nameservers tried)
	  (dns-query-no-cache query protocol nameservers tried)))

;; dns and recursion
;;  recursion means, if the demanded information is not available from the
;;  nameserver, another nameserver (usualy an authority) has to be contacted.
;;  normally the recursion is done for us by the nameserver istself, but
;;  this feature is technically optional (RFC 1035).
;;  dns-get-information implements the resovler-side recursion.
;;  it returns a dns-message
(define (dns-get-information query protocol check-answer . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let lp ((tried '()) (nss nameservers))
      (if (null? nss)
	  (if (null? tried)
	      (dns-error 'no-nameserver-given)
	      (dns-error 'bad-address))
	  (receive (dns-msg hit-ns nss-with-no-reply)
	      (dns-query/cache query use-cache? protocol nss tried)
	    (if (check-answer dns-msg)
		dns-msg
		(let ((auth? (flags-authoritative? (header-flags
						    (message-header
						     (dns-message-reply dns-msg))))))
		  (if auth?
		      (dns-error 'bad-address)
		      ;; other nameservers names are found in the nameserver-part,
		      ;; but their ip-adresses are found in the additonal-rrs
		      (let ((other-nameservers 
			     (filter (lambda (elem) (eq? (resource-record-type elem) (message-type a)))
				     (message-additionals (dns-message-reply dns-msg)))))
			(lp (if (not (member hit-ns tried)) (cons hit-ns tried) tried)
			    (lset-union equal?
					nss-with-no-reply
					(lset-difference equal? other-nameservers tried))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing of /etc/resolv.conf

(define-condition-type 'resolv.conf-parse-error '(dns-error))
(define resolv.conf-parse-error? (condition-predicate 'resolv.conf-parse-error))

(define (parse-nameserver rest-of-line)
  (let ((match (regexp-search
		(rx (: (submatch (** 1 3 digit) "."
				 (** 1 3 digit) "."
				 (** 1 3 digit) "."
				 (** 1 3 digit))
		       (* white))); don't complain about tailing white space
		rest-of-line)))
    (if match
	(cons 'nameserver (match:substring match 1))
	(signal 'resolv.conf-parse-error))))

; could be more restrictive...
(define domain-name-regexp (rx (+ (| alphanum #\. #\-))))

(define (parse-domain rest-of-line)
  (let ((match (regexp-search
		(rx (: (submatch ,domain-name-regexp)
		       (* white))); don't complain about tailing white space
		rest-of-line)))
    (if match
	(cons 'domain (match:substring match 1))
	(signal 'resolv.conf-parse-error))))

(define (parse-search rest-of-line)
  (let ((domains (regexp-fold-right domain-name-regexp 
				    (lambda (match junk accu)
				      (cons (match:substring match 0) accu))
				    '()
				    rest-of-line)))
    (if (null? domains)
	(signal 'resolv.conf-parse-error)
	(cons 'search domains))))

(define (parse-sortlist rest-of-line)
  (let ((netmask-pairs (regexp-fold-right (rx (+ (| digit #\. #\/)))
				    (lambda (match junk accu)
				      (cons (match:substring match 0) accu))
				    '()
				    rest-of-line)))
    (if (null? netmask-pairs)
	(signal 'resolv.conf-parse-error)
	(cons 'sortlist netmask-pairs))))

(define (parse-options rest-of-line)
  (regexp-fold-right 
   (rx (| "debug" "no_tld_query" (: "ndots:" (submatch digit))))
   (lambda (match junk accu)
     (let ((str (match:substring match 0)))
       (cond ((string=? str "debug")
	      (cons 'debug accu))
	     ((string=? str "no_tld_query")
	      (cons 'no_tld_query accu))
	     (else (cons (cons 'ndots 
			       (string->number (match:substring match 1))) accu)))))
   '()
   rest-of-line))

(define *resolv.conf-cache*)
(define *resolv.conf-cache-date* 0)
(define *resolv.conf-file* "/etc/resolv.conf")

(define (resolv.conf)
  (let ((actual-m-time (file-info:mtime (file-info *resolv.conf-file*))))
    (if (> actual-m-time *resolv.conf-cache-date*)
	(parse-resolv.conf!))
    *resolv.conf-cache*))

(define (parse-resolv.conf!)
  (let ((actual-m-time (file-info:mtime (file-info *resolv.conf-file*)))
	(contents (really-parse-resolv.conf *resolv.conf-file*)))
    (set! *resolv.conf-cache* contents)
    (set! *resolv.conf-cache-date* actual-m-time)))
  
(define (really-parse-resolv.conf file-name)

  ;; accumulate nameserver entries
  ;; domain and search are mutual exclusive, take the last
  (define (adjust-result rev-result have-search-or-domain? nameservers)
    (cond ((null? rev-result)
	   (if (null? nameservers)
	       '()
	       (list (cons 'nameserver nameservers))))
	  ((eq? (caar rev-result) 'domain)
	   (if have-search-or-domain?
	       (adjust-result (cdr rev-result) have-search-or-domain? nameservers)
	       (cons (car rev-result) 
		     (adjust-result (cdr rev-result) 
				    #t 
				    nameservers))))
	  ((eq? (caar rev-result) 'search)
	   (if have-search-or-domain?
	       (adjust-result (cdr rev-result) have-search-or-domain? nameservers)
	       (cons (car rev-result) 
		     (adjust-result (cdr rev-result) 
				    #t 
				    nameservers))))
	  ((eq? (caar rev-result) 'nameserver)
	   (adjust-result (cdr rev-result) 
			  have-search-or-domain? 
			  (cons (cdar rev-result)
				nameservers)))
	  (else (cons (car rev-result)
		      (adjust-result (cdr rev-result) 
				     have-search-or-domain? 
				     nameservers)))))

  (with-input-from-file file-name
    (lambda ()
      (let loop ((rev-result '()))
	(let ((l (read-line)))
	  (cond
	   ((eof-object? l)
	    (adjust-result rev-result #f '()))
	   ((regexp-search
	     (rx (: bos (| "#" ";")))
	     l)
	    (loop rev-result))
	   ((regexp-search
	     (rx (: bos "nameserver" (+ (| " " "\t")
				    (submatch (* any))
				    eos)))
	     l)
	    => (lambda (match)
		 (loop (cons (parse-nameserver (match:substring match 1))
			     rev-result))))
	   ((regexp-search
	     (rx (: bos "domain" (+ (| " " "\t")
				(submatch (* any))
				eos)))
	     l)
	    => (lambda (match)
		 (loop (cons (parse-domain (match:substring match 1))
			     rev-result))))
	   ((regexp-search
	     (rx (: bos "search" (+ (| " " "\t")
				(submatch (* any))
				eos)))
	     l)
	    => (lambda (match)
		 (loop (cons (parse-search (match:substring match 1))
			     rev-result))))
	     
	   ((regexp-search
	     (rx (: bos "sortlist" (+ (| " " "\t")
				  (submatch (* any))
				  eos)))
	     l)
	    => (lambda (match)
		 (loop (cons (parse-sortlist (match:substring match 1))
			     rev-result))))


	   ((regexp-search
	     (rx (: bos "options" (+ (| " " "\t")
				 (submatch (* any))
				 eos)))
	     l)
	    => (lambda (match)
		 (loop (cons (parse-options (match:substring match 1))
			     rev-result))))
	   ;; skips lines with parse errors, instead of 
	   ;; raising a 'resolv.conf-parse-error
	   (else (loop rev-result))))))))
					

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figure out the default name servers

(define (dns-find-nameserver-list)
  (cond ((assoc 'nameserver (resolv.conf))
	 => (lambda (nameserver.list)
	      (cdr nameserver.list)))
	(else '("127.0.0.1"))))

;; returns the first found nameserver
(define (dns-find-nameserver)
  (let ((ns (dns-find-nameserver-list)))
    (if (null? ns)
	(dns-error 'no-nameservers)
	(car ns))))

;; checks if the nameservers are working, prints a summary
(define (dns-check-nameservers . args)
  (let* ((print-summary
	  (lambda (working-channels non-working-channels)
	    (for-each (lambda (channel)
		       (display "FAIL: ")(display channel)
		       (display " - host not reachable.")(newline))
		     non-working-channels)
	    (for-each (lambda (channel)
		       (display "PASS: ")(display channel)
		       (display " - connection established.")(newline))
		     working-channels)
	    (if (null? working-channels)
		(begin (display "ERROR: no working nameserver found.")(newline)))))

	 (nameservers (if (null? args)
			  (dns-find-nameserver-list)
			  (car args)))
	 (sockets  (map
		    (lambda (nameserver)
		      (let ((sock (create-socket protocol-family/internet
						 socket-type/stream))
			    (addr (internet-address->socket-address
				   (ip-string->address32 nameserver) 53)))
			(call-with-current-continuation
			 (lambda (k)
			   (with-handler (lambda (cond more)
					   (display "FAIL: ")
					   (display nameserver)
					   (display " - no DNS Service available.")
					   (newline)
					   (k #f))
					 (lambda ()
					   (connect-socket-no-wait sock addr)
					   sock))))))
		    nameservers)))
    
    (let* ((nameservers
	    (let loop ((sockets sockets)
		       (nss nameservers))
	      (cond 
	       ((or (null? sockets) (null? nss)) '())
	       ((socket? (car sockets))
		(cons (car nss) (loop (cdr sockets) (cdr nss))))
	       (else (loop (cdr sockets) (cdr nss))))))

	   (sockets (filter socket? sockets))
	   (ws (map socket:outport sockets))
	   (wport-nameserver-alist (map cons ws nameservers))
	   (wport-socket-alist (map cons ws sockets)))
      (dynamic-wind
       (lambda () 
	 'nothing-to-be-done-before)
       (lambda ()
	 (let loop-port-channels ((working-channels '())
				  (non-working-channels '())
				  (tried-channels '())
				  (number-tries 1))
	   (letrec ((delete-list
		     (lambda (elems list)
		       (cond
			((null? elems) list)
			((null? list) '())
			(else (delete-list (cdr elems) (delete (car elems) list))))))
		    (ready (delete-list tried-channels
				   (apply select-port-channels *timeout* ws))))

	     (if (or (>= number-tries *max-tries*))
		 (print-summary working-channels 
				(delete-list working-channels 
					     (delete-list non-working-channels 
							  nameservers)))
		 (let loop-ready-channels ((working-channels working-channels)
					   (non-working-channels non-working-channels)
					   (ready-channels ready))
		   (if (null? ready-channels)
		       (loop-port-channels working-channels
					   non-working-channels
					   (append tried-channels ready)
					   (+ number-tries 1))
		       (let* ((w (car ready-channels))
			      (hit-ns (cdr (assoc w wport-nameserver-alist)))
			      (sock (cdr (assoc w wport-socket-alist))))
			 (if (connect-socket-successful? sock)
			     (loop-ready-channels (append working-channels (list hit-ns))
						  non-working-channels (cdr ready-channels))
			     (loop-ready-channels working-channels
						  (append non-working-channels 
							  (list hit-ns))
						  (cdr ready-channels))))))))))
       (lambda ()
	 (for-each close-socket sockets))))))


;; computes the nameservers argument of the lookup functions.
;; if a nameserver-name is given and not a nameserver-ip
;; (dns-lookup-name nameserver) is called.
;; use-cache? defaults to #t
(define (lookup-optional-args args)
  (if (null? args)
      (values (map ip-string->address32 (dns-find-nameserver-list)) #t)
      (values
       (map (lambda (nameserver)
	      (cond
	       ((address32? nameserver) nameserver)
	       ((ip-string? nameserver) (ip-string->address32 nameserver))
	       (else (map dns-lookup-name (dns-find-nameserver-list)))))
	    (car args))
       (if (null? (cdr args))
	   #t
	   (cadr args)))))

;; dns-lookup with more options than dns-lookup-*
(define (dns-lookup name type . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let* ((maybe-ip-string (if (address32? name) 
				(ip-string->in-addr.arpa-string (address32->ip-string name))
				(ip-string->in-addr.arpa-string name)))
	   (query (if maybe-ip-string	; if name is a ip-addr, the query is a in-addr.arpa address
		      (make-simple-query-message
		       maybe-ip-string type (message-class in))
		      (make-simple-query-message name type (message-class in))))
	   (protocol (network-protocol udp))
	   (check-answer (lambda (dns-msg) #t))
	   (dns-msg (dns-get-information query protocol check-answer nameservers use-cache?))
	   (answers (message-answers (dns-message-reply dns-msg))))
      dns-msg)))


;; looks up a hostname, returns an ip.
;; (dns-lookup-name <name> nameservers)
(define (dns-lookup-name name . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let* ((maybe-ip-string (if (address32? name) 
				(ip-string->in-addr.arpa-string (address32->ip-string name))
				(ip-string->in-addr.arpa-string name)))
	   (query (if maybe-ip-string	; if name is a ip-addr, the query is a in-addr.arpa address
		      (dns-error 'not-a-hostname)
		      (make-simple-query-message name (message-type a) (message-class in))))
	   (protocol (network-protocol udp))
	   (check-answer (lambda (dns-msg) 
			   (let* ((reply (dns-message-reply dns-msg))
				  (answers (message-answers reply)))
			     (not (null? (filter-type answers (message-type a)))))))
	   (dns-msg (dns-get-information query protocol check-answer nameservers use-cache?))
	   (answers (filter-type (message-answers (dns-message-reply dns-msg)) (message-type a))))
      (resource-record-data-a-ip (resource-record-data (car answers))))))

;; looks up an ip, returns a hostname
;; (dns-inverse-lookup <name> [nameserver])
(define (dns-lookup-ip ip . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let* ((maybe-ip-string (if (address32? ip) 
				(ip-string->in-addr.arpa-string (address32->ip-string ip))
				(ip-string->in-addr.arpa-string ip)))
	   (query (if maybe-ip-string	; if name is a ip-addr, the query is a in-addr.arpa address
		      (make-simple-query-message maybe-ip-string (message-type ptr) (message-class in))
		      (dns-error 'not-a-ip)))
	   (protocol (network-protocol udp))
	   (check-answer (lambda (dns-msg) 
			   (let* ((reply (dns-message-reply dns-msg))
				  (answers (message-answers reply)))
			     (not (null? (filter-type answers (message-type ptr)))))))
	   (dns-msg (dns-get-information query protocol check-answer nameservers use-cache?))
	   (answers (filter-type (message-answers (dns-message-reply dns-msg)) (message-type ptr))))
      (resource-record-data-ptr-name (resource-record-data (car answers))))))

(define dns-inverse-lookup dns-lookup-ip)

;; looks up an authoritative nameserver for a hostname
;; returns a list of nameservers
;; (dns-lookup-nameserver <name> [nameserver])
(define (dns-lookup-nameserver name . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let* ((maybe-ip-string (if (address32? name) 
				(ip-string->in-addr.arpa-string (address32->ip-string name))
				(ip-string->in-addr.arpa-string name)))
	   (query (if maybe-ip-string	; if name is a ip-addr, the query is a in-addr.arpa address
		      (dns-error 'not-a-hostname)
		      (make-simple-query-message
		       name (message-type ns) (message-class in))))
	   (protocol (network-protocol udp))
	   (check-answer (lambda (dns-msg) 
			   (let* ((reply (dns-message-reply dns-msg))
				  (answers (message-answers reply))
				  (nameservers (message-nameservers reply)))
			     (or (not (null? (filter-type nameservers (message-type soa))))
				 (not (null? (filter-type answers (message-type ns))))))))
	   (dns-msg (dns-get-information query protocol check-answer nameservers use-cache?))
	   (reply (dns-message-reply dns-msg))
	   (soa (filter-type (message-nameservers reply) (message-type soa)))
	   (nss (filter-type (message-answers reply) (message-type ns)))
	   (add (filter-type (message-additionals reply) (message-type a))))
      (if (null? nss)
	  (list (dns-lookup-name (resource-record-data-soa-mname (resource-record-data (car soa)))))
	  (map (lambda (elem) (resource-record-data-a-ip (resource-record-data elem))) add)))))

;; looks up a mail-exchanger for a hostname.
;; returns a list of mail-exchanger, sorted by their preference
;; if there are no mx-records in the answer-section, 
;; implementation based on RFC2821
;; (dns-lookup-mail-exchanger <name> [nameserver])
(define (dns-lookup-mail-exchanger name . args)
  (receive (nameservers use-cache?) (lookup-optional-args args)
    (let* ((ip-string (if (address32? name) 
			  (ip-string->in-addr.arpa-string (address32->ip-string name))
			  (ip-string->in-addr.arpa-string name)))
	   (query (if ip-string		; if name is a ip-addr, the query is a in-addr.arpa address
		      (dns-error 'not-a-hostname)
		      (make-simple-query-message
		       name (message-type mx) (message-class in))))
	   (protocol (network-protocol tcp))
	   (check-answer (lambda (dns-msg) 
			   (let* ((reply (dns-message-reply dns-msg))
				  (answers (message-answers reply))
				  (nameservers (message-nameservers reply)))
			     (or (not (null? (filter-type answers (message-type mx))))
				 (not (null? (filter-type answers (message-type cname))))
				 (not (null? (filter-type answers (message-type a))))))))
	   (dns-msg (dns-get-information query protocol check-answer nameservers use-cache?))
	   (reply (dns-message-reply dns-msg))
	   (mx (filter-type (message-answers reply) (message-type mx)))
	   (soa (filter-type (message-nameservers reply)(message-type soa)))
	   (cname (filter-type (message-answers reply) (message-type cname)))
	   (a (filter-type (message-answers reply) (message-type a))))

      (cond
       ((not (null? a)) 
	(list (resource-record-data-a-ip (resource-record-data (car a)))))
       ((not (null? cname))
	(dns-lookup-mail-exchanger (resource-record-data-cname-name (resource-record-data (car cname)))))
       ((null? mx) 
	(list (resource-record-data-soa-rname (resource-record-data (car soa)))))
       (else
	(map (lambda (elem) (resource-record-data-mx-exchanger (resource-record-data elem))) (sort-by-preference mx)))))))

;;; pretty-prints a dns-msg
(define (pretty-print-dns-message dns-msg . maybe-port)
  (let ((d 
	 (lambda (n s1 s2)
	   (letrec ((loop (lambda (n)
			    (if (zero? n)
				""
				(string-append " " (loop (- n 1)))))))
	     (display (loop n))
	     (display s1)
	     (display ": ")
	     (display s2)
	     (newline)))))
    (with-current-output-port*
     (if (null? maybe-port)
	 (current-output-port)
	 (car maybe-port))
     (lambda ()
       (define (show-dns-message dns-msg)
	 (cond
	  ((dns-message? dns-msg) 
	   (begin
	     (d 0 "DNS-MESSAGE" "")
	     (d 1 "QUERY" "")(show-dns-message (dns-message-query dns-msg))(newline)
	     (d 1 "REPLY" "")(show-dns-message (dns-message-reply dns-msg))(newline)
	     (d 1 "CACHE?" (if (dns-message-cache? dns-msg)
			       "found in cache"
			       "not found in cache"))
	     (d 1 "PROTOCOL" (let ((protocol (dns-message-protocol dns-msg)))
			       (cond
				((eq? protocol (network-protocol tcp)) "TCP")
				((eq? protocol (network-protocol udp)) "UDP"))))
	     (d 1 "TRIED-NAMESERVERS" (if (> (length (dns-message-tried-nameservers dns-msg)) 1)
					  (begin
					    (display " had perform recursion: ")
					    (dns-message-tried-nameservers dns-msg))
					  (begin
					    (display " without recursion: ")
					    (dns-message-tried-nameservers dns-msg))))))
	  ((message? dns-msg)
	   (begin
	     (d 2 "MESSAGE" "")
	     (d 3 "Header     " "")(show-dns-message (message-header dns-msg))
	     (d 3 "Questions  " "")(for-each (lambda (x) (show-dns-message x)(newline)) (message-questions dns-msg))
	     (d 3 "Answers    " "")(for-each (lambda (x) (show-dns-message x)(newline)) (message-answers dns-msg))
	     (d 3 "Nameservers" "")(for-each (lambda (x) (show-dns-message x)(newline)) (message-nameservers dns-msg))
	     (d 3 "Additionals" "")(for-each (lambda (x) (show-dns-message x)(newline)) (message-additionals dns-msg))))
	  ((header? dns-msg)
	   (begin
	     (d 4 "id" (header-id dns-msg))
	     (d 4 "Flags" "")(show-dns-message (header-flags dns-msg))
	     (d 4 "question-count    " (header-question-count dns-msg))
	     (d 4 "answer-count      " (header-answer-count dns-msg))
	     (d 4 "nameserver-count  " (header-nameserver-count dns-msg))
	     (d 4 "additional-count  " (header-additional-count dns-msg))))
	  ((flags? dns-msg)
	   (begin 
	     (d 5 "querytype" (flags-query-type dns-msg))
	     (d 5 "opcode" (flags-opcode dns-msg))
	     (d 5 "authoritative?" (flags-authoritative? dns-msg))
	     (d 5 "truncated?" (flags-truncated? dns-msg))
	     (d 5 "recursion-desired?" (flags-recursion-desired? dns-msg))
	     (d 5 "recursion-available?" (flags-recursion-available? dns-msg))
	     (d 5 "zero" (flags-zero dns-msg))
	     (d 5 "response-code" (flags-response-code dns-msg))))
	  ((question? dns-msg)
	   (begin
	     (d 4 "name " (question-name dns-msg))
	     (d 4 "type " (message-type-name (question-type dns-msg)))
	     (d 4 "class" (message-class-name (question-class dns-msg)))))
	  ((resource-record? dns-msg)
	   (begin
	     (d 4 "name " (resource-record-name dns-msg))
	     (d 4 "type "(message-type-name (resource-record-type dns-msg)))
	     (d 4 "class" (message-class-name (resource-record-class dns-msg)))
	     (d 4 "ttl  " (resource-record-ttl dns-msg))
	     (d 4 "data " "") (show-dns-message (resource-record-data dns-msg))))
	  ((resource-record-data-a? dns-msg)
	   (d 5 "ip " (resource-record-data-a-ip dns-msg)))
	  ((resource-record-data-ns? dns-msg)
	   (d 5 "name " (resource-record-data-ns-name dns-msg)))
	  ((resource-record-data-cname? dns-msg)
	   (d 5 "name " (resource-record-data-cname-name dns-msg)))
	  ((resource-record-data-mx? dns-msg)
	   (begin
	     (d 5 "preference " (resource-record-data-mx-preference dns-msg))
	     (d 5 "exchanger  " (resource-record-data-mx-exchanger dns-msg))))
	  ((resource-record-data-ptr? dns-msg) 
	   (d 5 "name " (resource-record-data-ptr-name dns-msg)))
	  ((resource-record-data-soa? dns-msg)
	   (begin
	     (d 5 "mname   " (resource-record-data-soa-mname dns-msg))
	     (d 5 "rname   " (resource-record-data-soa-rname dns-msg))
	     (d 5 "serial  " (resource-record-data-soa-serial dns-msg))
	     (d 5 "refresh " (resource-record-data-soa-refresh dns-msg))
	     (d 5 "expire  " (resource-record-data-soa-expire dns-msg))
	     (d 5 "minimum " (resource-record-data-soa-expire dns-msg))))
	  ;; ###
	  ((resource-record-data-hinfo? dns-msg)
	   (d 5 "data " (resource-record-data-hinfo-data dns-msg)))
	  ((resource-record-data-txt? dns-msg)
	   (d 5 "text " (resource-record-data-txt-text dns-msg)))
	  ((resource-record-data-wks? dns-msg)
	   (d 5 "data " (resource-record-data-wks-data dns-msg)))
	  ))
       (show-dns-message dns-msg)))))

(define (socket-address->fqdn addr . args)
  (receive (ip32 port)
      (socket-address->internet-address addr)
    (apply dns-lookup-ip ip32 args)))

;; Some code to test the components of domain names
;;(define label-regexp
;;  (rx (: alpha (? (* (| alphanumeric "-")) alphanumeric))))
;;(define (unqualified-hostname name)
;;  (regexp-search? (rx (: ,label-regexp)) name))


(define (maybe-dns-lookup-name name . args)
  (call-with-current-continuation
   (lambda (k)
     (with-handler (lambda (cond more)
		     (if (dns-error? cond)
			 (k #f)
			 (more)))
		   (lambda ()
		     (apply dns-lookup-name name args))))))

(define (maybe-dns-lookup-ip ip-addr . args)
  (call-with-current-continuation
   (lambda (k)
     (with-handler (lambda (cond more)
		     (if (dns-error? cond)
			 (k #f)
			 (more)))
		   (lambda ()
		     (apply dns-lookup-ip ip-addr args))))))

(define (domains-for-search)
  (cond ((assoc 'domain (resolv.conf))
	 => (lambda (pair)
	      (list (cdr pair))))
	((assoc 'search (resolv.conf))
	 => (lambda (pair)
	      (cdr pair)))
	(else '())))

(define (host-fqdn name-or-socket-address . args)
  (if (socket-address? name-or-socket-address)
      (apply socket-address->fqdn name-or-socket-address args)
      (let ((name name-or-socket-address))
	(cond ((apply maybe-dns-lookup-name name args)
	       => (lambda (ip)
		    (apply dns-lookup-ip ip args)))
	      (else
	       (let lp ((domains (domains-for-search)))
		 (if (null? domains)
		     #f
		     (cond ((apply maybe-dns-lookup-name 
				   (string-append name "." (car domains)) args)
			    => (lambda (ip)
				 (apply dns-lookup-ip ip args)))
			   (else (lp (cdr domains)))))))))))

(define (system-fqdn . args)
  (apply host-fqdn (system-name) args))

