#! /usr/local/bin/gsi -f
":"; exec /usr/local/bin/gsi -f $0
;
; 		This is a CGI script to search an
;	Air Weather Service Master Station Library Catalog
;
; Synopsis
; 	REQUEST_METHOD: GET or POST
; 	A QUERY_STRING or a POST message may specify one or several parameters,
; as name=value pairs. A value sometimes may be omitted; furthermore,
; for certain names the value is irrelevant: the mere occurrence of this
; particular name is important (rather than the value that may be associated
; with it); we will call such a name key-name or a key parameter.
;
; Depending on which parameters are specified (or if any are specified at
; all) the script operates in one of the following modes:
;
; 1) show form mode: no parameters are given at all
;    This mode simply presents a search form: an HTML document that lets a
;    user specify which information (fields) of the MStationLib table to search
;    for
;    This mode is the end point of almost any other modes below.
;
; 2) Search for a station/stations: a 'do-search' parameter must be given.
;    In addition, one or more of the following parameters should be specified
;    to tell search criteria:
;	block_id= block_id of a station to search for
;	call_id=  call letters of the station
;	name=     partial name of the station; may contain SQL-style wildcards.
;		  If it does not, a "%" wildcard is appended to the given name.
;	country_spec_fmt= either "code" or "name" - tells how country search
;		is to be performed
;	country_spec= a two letter country code of a full country name,
;		possibly containing SQL-style wildcards.
;		If it does not, a "%" wildcard is appended to the given name.
;	lat-n=  specifies lat boundaries for an area we search stations in.
;	lat-s=  the northern-most and the southern-most latitudes.
;		Both may be omitted, otherwise both must be specified,
;		  and lat-n => lat-s
;	lon-e=  longitudal search constraints; the eastern-most and the
;	lon-w=  western-most points of the area of interest. Note, that
;		lon-e may be either greater or _less_ than lon-w
;
;    The search is case-insensitive. If several criteria are specified,
;    they are assumed to be ANDed
;
; This version of search-mslib.scm is an example of Web services. A
; previous version of this program used to connect to MetcastDB
; directly and query it using SQL. In contrast, this version relies on a
; Metcast server -- a web application server -- to do the query. The
; program search-mslib.scm connects to a Metcast server via HTTP and
; submits an MBL request.  MBL is an abstract syntax representation of
; an XML request. If the request was successful, the Metcast server
; returns an XML element or elements, e.g.,
;
;	<msl><id>724915</id><call-id>KMRY</call-id>
;	<name>MONTEREY PENINSULA</name><cc>US</cc><lat>36.58333200000</lat>
;	<lon>-121.850000000</lon><elev>77</elev>
;	<rw-dir></rw-dir><last-mod>05/05/1998</last-mod>
;	<country>UNITED STATES OF AMERICA</country></msl>
;
; See metar.scm for more details. After search-mslib.scm receives the
; above collection of XML elements, it converts them to SXML.
; The SXML code will be later converted to HTML to be placed in an
; "inline frame" on the reply web page.
;
; The current program relies extensively on SXML to express all HTML
; content (in many high-order, high-abstraction tags).
; See http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-authoring
; for more examples and explanation.


; $Id: search-mslib.scm,v 3.3 2002/09/28 05:41:58 oleg Exp oleg $

(declare 
 (block)
 (standard-bindings)
 (fixnum)
)

(include "myenv.scm")
(include "client-conf.scm")
(include "cgi-common.scm")
(include "SXML-tree-trans.scm")
(include "mime.scm")
;(include "http.scm")
(include "mbl-query.scm")

		; Local configuration parameters
		; Limit the result of a query to this many rows
(define MAX-rows-return 20)

;-------------------------
; The main CGI form


(define Form
  `(html:begin "Searching the Master Station Library"
     (body (@ (BGCOLOR "#DFFFDF") (TEXT "#000000")
	      (LINK "#0000FF") (VLINK "#663399")
	      (LEFTMARGIN "8") (TOPMARGIN "8"))
       (h1 "Searching the Master Station Library")
       (div (note-short))

       (search-form "Specify search parameters"
	 (table (@ (cellpadding "2") (cellspacing "5") (border "0"))
	     (tr (th "Station WMO ID")
		 (td (ffield-input-text block_id int 6)))
	     (tr (th "Call letters")
		 (td (ffield-input-text call_id token 5)
		     (br)
		     "e.g., " (code "ksfo")))
	     (tr (th "Full or partial name of a station")
		 (td (ffield-input-text name string 40)
		     (br)
		     "e.g., " (code "San Franci")))
	     (tr (td (ffield-select country_spec_fmt
			(ffield-select-option "code"
					      "2-letter country code")
			(ffield-select-option "name"
					      "The name of the country")
			))
		 (td (ffield-input-text country_spec string 40 
					(maxlength 70))))
	     (tr (th "Latitude, deg")
		 (td "between "
		     (ffield-input-text lat-s number 6)
		     " and "
		     (ffield-input-text lat-n number 6)))
	     (tr (th "Longitude, deg")
		 (td "between "
		     (ffield-input-text lon-w number 7)
		     " and "
		     (ffield-input-text lon-e number 7)))
	     )
	 (p "All character search is case-insensitive. SQL-style wildcards "
	    "(" (code "%") ", " (code "_") ") are OK.")
	 (p "Lat/Lon are specified in degrees, e.g. "
	    (code "-30.75") "; "
	    "negative numbers refer to Southern latitudes and Western "
	    "longitudes correspondingly. "
	    (em "Longitudes may wrap around") ".")
	 (p "You have to specify at least one search parameter.")
	 (p "Metcast server to query: "
	    (ffield-input-text mserver_url token 60 (maxlength 80)
			       (value ,METCAST-SERVER-URL)))
	 (ffield-submit do-search "Find")
	 (p "Refer to the " (a (@ (href ,URL:HELP)) "Metcast Site")
	    " for more information.")
	 )
       (p (hr))
       (note-long)
       ))
)




; The show mode

; The arg-res may be either an SXML value (a string or a list), or a
; procedure generating an SXML value. In the former case, the arg-res
; is inserted before the body of the form. In the latter case, the
; procedure is called after the form is written.

(define (mode-show-form arg-res)
  (SRV:send-reply
   (post-order
    (pre-post-order Form
      `(		       ; The stylesheet
	,@alist-conv-rules		; the identity rules
     
	(note-short		; A short note 
	 . ,(lambda (tag)
	      (and (not (procedure? arg-res)) arg-res)))
	
	(note-long		; A longer note (usually the result of a query)
	 . ,(lambda (tag)
	      (and (procedure? arg-res) (arg-res))))

	(search-form		; (search-form "title" body ...)
	 . ,(lambda (tag title . body)
	      `(form (@ (method "post")
			(action ,(CGI:lookup 'self-url 'token)))
		     (h3 (@ (align "center")) ,title)
		     ,body)))

        ; Fields of the form

	; (ffield-input-text name type size [others...])
	; where 'others' are:
	;	(value xxx)	-- default value
	;	(maxlength xxx) -- max length
	(ffield-input-text 
	 *preorder*
	 . ,(lambda (tag name type size . other-args)
	      `(input (@ (type "TEXT")
			 (name ,(symbol->string name))
			 (value 
			  ,(or (CGI:lookup name 'io #f)
			       (lookup-def 'value other-args "")))
			 (size ,size)
			 ,(or (assq 'maxlength other-args)
			      (list 'maxlength size))))))

	(ffield-select
	 *preorder*
	 . ,(lambda (tag name . options)
	      (let ((given-value (CGI:lookup name 'token "")))
		`(select (@ (name ,(symbol->string name)))
			 ,(map
			   (lambda (option) ; (tag value . body)
			     (assert (pair? option)
				     (eq? 'ffield-select-option (car option))
				     (pair? (cdr option)))
			     (let ((value (cadr option)))
			       `(option (@ (value ,value)
					   ,(and
					     (equal? given-value value)
					     "SELECTED"))
					,(cddr option))))
			   options)))))

	(ffield-submit	; (ffield-submit name value)
	 *preorder*
	 . ,(lambda (tag name value)
	      `(input (@ (type "SUBMIT") (name ,(symbol->string name))
			 (value ,value)))))
	))
    generic-web-rules)))



; Converting the CGI environment into an MBL request
;
; From another point of view, we re-write the Form into an MBL
; request, looking up the values of needed variables in the CGI
; environment.
;
; The MBL request will look as follows:
; (webq
;  (bounding-box 90.0 -180 -90 180)
;  (max-records 20)
;  (block_id "1")
;  (call_id "KMRY")
;  (st_name "Monterey%")
;  (st_country_code "US")
;  (st_country_name "%United%")
;  (mime-type "text/plain")
;  (products (MSL)))
; Of course, not all these parameters need be present.
;
; The procedure returns the MBL request: an S-expression.
; If an error is detected, the function make-mbl-request may 
; invoke the esc-to-show continuation passing it an SXML
; expression describing the error.

(define (make-mbl-request esc-to-show)
  ;(cerr "query-string: " (CGI:lookup 'query-parms 'token) nl)
  (let* 
      ((vars			; Bindings for all CGI vars that are
				; mentioned in the 'Form'
	(with-exception-handler
	 (lambda (exn)
	   (if ((condition-predicate 'cgi-type-error) exn)
	       (esc-to-show
		`(b "parameter " 
		    (code
		     ,(symbol->string
		       ((condition-property-accessor 'cgi-type-error 'name)
			exn)))
		    " must be of a type "
		    (code 
		     ,(symbol->string
		       ((condition-property-accessor 'cgi-type-error 'type)
			exn)))))
	       (CGI:exception-handler exn)))
	 (lambda ()
	   (pre-post-order Form
		; The re-writing rules, from Form to a CGI env bindings
	    `((*default*
	       . ,(lambda (attr-key . elems)
		    ;(cerr "default: " attr-key elems nl)
		    (let loop ((elems elems) (res '()))
		      (cond
		       ((null? elems) res)
		       ((null? (car elems)) (loop (cdr elems) res))
		       ((not (pair? (car elems))) (loop (cdr elems) res))
		       ((pair? (caar elems))
			(loop (cdr elems) (loop (car elems) res)))
		       (else (loop (cdr elems) (cons (car elems) res)))))))
	      (*text* . ,(lambda (trigger str) '()))

	      (ffield-input-text
	       *preorder*
	       . ,(lambda (tag name type size . other-args)
		    (let ((val (CGI:lookup name type #f)))
		      (if val (cons name val) '()))))

	      (ffield-select
	       *preorder*
	       . ,(lambda (tag name . options)
		    (cons name (CGI:lookup name 'token))))
	      )))))

       (lat-n (lookup-def 'lat-n vars #f))
       (lat-s (lookup-def 'lat-s vars #f))
       (lon-e (lookup-def 'lon-e vars #f))
       (lon-w (lookup-def 'lon-w vars #f))
       (bounding-box
	(begin
	  (if (not (eq? (not lat-n) (not lat-s)))
	      (esc-to-show
	       '(b "both latitudal boundaries must be specified or omitted")))
	  (if (and lat-s (not (<= -90.0 lat-s lat-n 90.0)))
	      (esc-to-show 
	       '(b "-90.0 <= lat-s <= lat-n <= 90.0 does not hold")))
	  (if (not (eq? (not lon-e) (not lon-w)))
	      (esc-to-show
	       '(b "both longitudal boundaries must be specified or omitted")))
	  (list (or lat-n 90.0) (or lon-w -180.0)
		(or lat-s -90.0) (or lon-e 180.0))))

       (block-id (lookup-def 'block_id vars #f))
       (call-id
	(let ((id (lookup-def 'call_id vars #f)))
	  (and id (string-upcase id))))

       (st-name
	(let ((name (lookup-def 'name vars #f)))
	  (and name
	       (string-append (string-upcase name)
			      (if (string-index name #\%) "" "%")))))
       (country-spec (lookup-def 'country_spec vars #f))
       (country-code
	(and country-spec
	     (equal? (lookup-def 'country_spec_fmt vars "code") "code")
	     (string-upcase country-spec)))
       (country-name
	(and country-spec
	     (not (equal? (lookup-def 'country_spec_fmt vars "code") "code"))
	     (string-append (string-upcase country-spec)
			    (if (string-index country-spec #\%) "" "%"))))
       )

    ;(pp vars ##stderr)

    (or lat-n lat-s lon-e lon-w 
	block-id call-id st-name country-spec
	(esc-to-show '(p (b "Nothing to search for"))))

    ; The answer -- an MBL request
    (let ((mbl-body
	   `((bounding-box ,@bounding-box)
	     (max-records ,MAX-rows-return)
	     (block_id ,(and block-id (number->string block-id)))
	     (call_id ,call-id)
	     (st_name ,st-name)
	     (st_country_code ,country-code)
	     (st_country_name ,country-name)
	     (mime-type "text/plain")
	     (products (MSL)))))
      ; remove associations with #f as the value
      (cons 'webq
	(let loop ((mbl-body mbl-body))
	  (cond
	   ((null? mbl-body) '())
	   ((and (pair? (car mbl-body)) (eq? #f (cadar mbl-body)))
	    (loop (cdr mbl-body)))
	   (else (cons (car mbl-body) (loop (cdr mbl-body)))))))
      )
))

; Querying the Metcast server
; Given an mbl expression that represents an MSL MBL query, execute the
; query and format the result.
; This function must return an SXML expression or a thunk that yields
; an SXML expression. They will be passed to mode-show-form.  The
; function may return prematurely by calling the esc-to-show
; continuation.


(define (mode-search mbl-request esc-to-show)
  ;(esc-to-show (with-output-to-string (lambda () (pp mbl-request))))
  (perform-MBL-request
   (CGI:lookup 'mserver_url 'token)
   mbl-request
   (lambda (headers port)
     (if (not port)
	 '(p (b "Nothing was found to satisfy your request"))
	 (format-msl-reply port))))
)


; The port contains a series of XML documents (elements) like the following
;
; <msl><id>724915</id><call-id>KMRY</call-id>
; <name>MONTEREY PENINSULA</name><cc>US</cc><lat>36.58333200000</lat>
; <lon>-121.850000000</lon><elev>77</elev>
; <rw-dir></rw-dir><last-mod>05/05/1998</last-mod>
; <country>UNITED STATES OF AMERICA</country></msl>
; <msl><id>710347</id><call-id>CYBW</call-id>
; <name>CALGARY SPRINGBANK</name><cc>CN</cc><lat>51.09999800000</lat>
; <lon>-114.366670000</lon><elev>1200</elev>
; <rw-dir></rw-dir><last-mod>01/12/1999</last-mod>
; <country>CANADA</country></msl>
;
; Convert that series to a table. Each <msl> element is converted to a row,
; each child element is converted to a column
; Return the SXML code for the table

(define (format-msl-reply port)
  (define table-attrs '(@ (border 1) (cellspacing 1) (cellpadding 5)))
  (define table-head
    '(tr (th "WMO ID") (th "Call Letters") (th "Name")
	 (th "Country Code") (th "lat") (th "lon") (th "Elevation")
	 (th "runway_dir") (th "Last Change") (th "Country Name")))

  (define (start-row port row-count prev-rows)
    (if (not (find-string-from-port? "<msl>" port))
	(finish-table row-count prev-rows)
	(let loop ((cols '()))
	  (skip-while '(#\newline #\space #\return) port)
	  (assert (eqv? #\< (read-char port)))
	  (if (eqv? #\/ (peek-char port)) ; must be </msl> tag
	      (start-row port (++ row-count)
			 (cons (list 'tr (reverse cols)) prev-rows))
	      (let ((col-value
		     (begin
		       (skip-until '(#\>) port)
		       (next-token '() '(#\<) "reading value" port))))
		(skip-until '(#\>) port)
		(loop (cons (list 'td 
			       (if (string-null? col-value) '(n_) col-value)) 
			    cols)))))))

  (define (finish-table row-count rows)
    (list
     (list 'table
	   table-attrs
	   table-head
	   rows)
     (if (= row-count MAX-rows-return)
	 (list "More stations are found, but only the first "
	       (list 'b row-count) " are shown")
	 (list (list 'b row-count) " station(s) were found"))))

  (start-row port 0 '())
)

;-----------------------------------
; 	The root thunk of the code

(with-exception-handler
 CGI:exception-handler
 (lambda ()
   (mode-show-form 
    (call-with-current-continuation
     (lambda (show-cont)
       (cond
	((eq? (CGI:lookup 'do-search 'token 'absent) 'absent)
	 '(p (n_)))
	(else (mode-search 
	       (make-mbl-request show-cont)
	       show-cont))))))))
