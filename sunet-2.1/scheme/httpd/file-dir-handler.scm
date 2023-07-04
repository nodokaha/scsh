;;; http server in the Scheme Shell	-*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; Copyright (c) 1996-2003 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define server/buffer-size 8192)	; WTF

(define-record-type file-directory-options :file-directory-options
  (really-make-file-directory-options file-name->content-type
				      file-name->content-encoding
				      file-name->icon-url
				      directory-icon-url
				      blank-icon-url
				      back-icon-url
				      unknown-icon-url)
  file-directory-options?
  (file-name->content-type file-directory-options-file-name->content-type
			   set-file-directory-options-file-name->content-type!)
  (file-name->content-encoding file-directory-options-file-name->content-encoding
			       set-file-directory-options-file-name->content-encoding!)
  (file-name->icon-url file-directory-options-file-name->icon-url 
		       set-file-directory-options-file-name->icon-url!)
  (directory-icon-url file-directory-options-directory-icon-url
		      set-file-directory-options-directory-icon-url!)
  (blank-icon-url file-directory-options-blank-icon-url
		  set-file-directory-options-blank-icon-url!)
  (back-icon-url file-directory-options-back-icon-url
		 set-file-directory-options-back-icon-url!)
  (unknown-icon-url file-directory-options-unknown-icon-url
		    set-file-directory-options-unknown-icon-url!))

(define (make-default-file-directory-options)
  (really-make-file-directory-options default-file-name->content-type
				      default-file-name->content-encoding
				      default-file-name->icon-url
				      #f #f #f #f))

(define (copy-file-directory-options options)
  (let ((new-options (make-default-file-directory-options)))
    (set-file-directory-options-file-name->content-type!
     new-options
     (file-directory-options-file-name->content-type options))
    (set-file-directory-options-file-name->content-encoding!
     new-options
     (file-directory-options-file-name->content-encoding options))
    (set-file-directory-options-file-name->icon-url!
     new-options
     (file-directory-options-file-name->icon-url options))
    (set-file-directory-options-directory-icon-url!
     new-options
     (file-directory-options-directory-icon-url options))
    (set-file-directory-options-blank-icon-url!
     new-options
     (file-directory-options-blank-icon-url options))
    (set-file-directory-options-back-icon-url!
     new-options
     (file-directory-options-back-icon-url options))
    (set-file-directory-options-unknown-icon-url!
     new-options
     (file-directory-options-unknown-icon-url options))
    new-options))

(define (make-file-directory-options-transformer set-option!)
  (lambda (new-value . stuff)
    (let ((new-options (if (not (null? stuff))
			   (copy-file-directory-options (car stuff))
			   (make-default-file-directory-options))))
      (set-option! new-options new-value)
      new-options)))

(define with-file-name->content-type
  (make-file-directory-options-transformer
   set-file-directory-options-file-name->content-type!))
(define with-file-name->content-encoding
  (make-file-directory-options-transformer
   set-file-directory-options-file-name->content-encoding!))
(define with-file-name->icon-url
  (make-file-directory-options-transformer
   set-file-directory-options-file-name->icon-url!))
(define with-blank-icon-url
  (make-file-directory-options-transformer
   set-file-directory-options-blank-icon-url!))
(define with-back-icon-url
  (make-file-directory-options-transformer
   set-file-directory-options-back-icon-url!))
(define with-unknown-icon-url
  (make-file-directory-options-transformer
   set-file-directory-options-unknown-icon-url!))

(define (make-file-directory-options . stuff)
  (let loop ((options (make-default-file-directory-options))
	     (stuff stuff))
    (if (null? stuff)
	options
	(let* ((transformer (car stuff))
	       (value (cadr stuff)))
	  (loop (transformer value options)
		(cddr stuff))))))

;;; (home-dir-handler user-public-dir) -> handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return a request handler that looks things up in a specific directory
;;; in the user's home directory. If ph = (home-dir-handler "public_html")
;;; then ph is a request handler that serves files out of peoples' public_html
;;; subdirectory. So
;;;	(ph '("shivers" "hk.html") req)
;;; will serve the file 
;;;     ~shivers/public_html/hk.html
;;; The request handler treats the URL path as (<user> . <file-path>),
;;; serving
;;;     ~<user>/<user-public-dir>/<file-path>

(define (home-dir-handler user-public-dir . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (lambda (path req)		
      (if (null? path)
	  (make-error-response (status-code bad-request)
			       req
			       "Path contains no home directory.")
	  (make-rooted-file-path-response (string-append (http-homedir (car path) req)
							 "/"
							 user-public-dir)
					  (cdr path)
					  file-serve-response
					  req
					  options)))))

;;; (tilde-home-dir-handler user-public-dir default-request-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If the car of the path is a tilde-marked home directory (e.g., "~kgk"),
;;; do home-directory service as in HOME-DIR-HANDLER, otherwise punt to the
;;; default handler.

(define (tilde-home-dir? path req)
  (and (not (null? path))
       (let ((head (car path)))		; home-directory path?
	 (and (> (string-length head) 0)
	      (char=? (string-ref head 0) #\~)))))

(define (tilde-home-dir-handler user-public-dir default-handler . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (make-predicate-handler
     tilde-home-dir?
     (lambda (path req)
       (let* ((tilde-home (car path))	; Yes.
	      (slen (string-length tilde-home))
	      (subdir (string-append
		       (http-homedir (substring tilde-home 1 slen) req)
		       "/"
		       user-public-dir)))
	 (make-rooted-file-path-response subdir (cdr path) file-serve-response req
					 options)))
     default-handler)))


;;; Make a handler that serves files relative to a particular root
;;; in the file system. You may follow symlinks, but you can't back up
;;; past ROOT with ..'s.

(define (rooted-file-handler root . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (lambda (path req)
      (make-rooted-file-path-response root path file-serve-response req options))))

;;; Dito, but also serve directory indices for directories without
;;; index.html. 

(define (rooted-file-or-directory-handler root . maybe-options)
  (let-optionals maybe-options ((options (make-default-file-directory-options)))
    (lambda (path req)
      (make-rooted-file-path-response root path
				      file-serve-and-dir-response
				      req
				      options))))


;;;; Support procs for the path handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (MAKE-ROOTED-FILE-PATH-RESPONSE root file-path req options)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do a request for a file. The file-name is determined by appending the
;;; the FILE-PATH list the string ROOT. E.g., if
;;;     ROOT = "/usr/shivers"	FILE-PATH = ("a" "b" "c" "foo.html")
;;; then we serve file
;;;     /usr/shivers/a/b/c/foo.html
;;; Elements of FILE-PATH are *not allowed* to contain .. elements.
;;;   (N.B.: Although the ..'s can appear in relative URI's, /foo/../ path 
;;;    sequences are processed away by the browser when the URI is converted
;;;    to an absolute URI before it is sent off to the server.)
;;; It is possible to sneak a .. past this kind of front-end resolving by
;;; encoding it (e.g., "foo%2F%2E%2E" for "foo/.."). If the client tries
;;; this, MAKE-ROOTED-FILE-PATH-RESPONSE will catch it, and abort the transaction.
;;; So you cannot make the reference back up past ROOT. E.g., this is
;;; not allowed:
;;;     FILE-PATH = ("a" "../.." "c" "foo.html")
;;;
;;; Only GET and HEAD ops are provided. 
;;; The URL's <search> component must be #f.
;;; The file is served if the server has read or stat(2) access to it,
;;; respectively. If the server is run as root, this might be a problem.
;;;
;;; FILE-SERVE is a procedure which gets passed the file name, the
;;; path, and the HTTP request to serve the file propert after the
;;; security checks.  Look in ROOTED-FILE-HANDLER and
;;; ROOTED-FILE-OR-DIRECTORY-HANDLER for examples on how to feed this.

(define (make-rooted-file-path-response root file-path file-serve-response req options)
  (if (http-url-search (request-url req))
      (make-error-response (status-code bad-request) req
			   "Indexed search not provided for this URL.")
      (cond ((dotdot-check root file-path) =>
	     (lambda (fname)
	       (file-serve-response fname file-path req options)))
	    (else
	     (make-error-response (status-code bad-request) req
				  "URL contains unresolvable ..'s.")))))


;; Just (file-info fname) with error handling.

(define (stat-carefully fname req)
  (with-errno-handler 
      ((errno packet)
       ((errno/noent)
	(http-error (status-code not-found) req))
       ((errno/acces)
	(http-error (status-code forbidden) req)))
    (file-info fname #t)))

;;; A basic file request handler -- ship the dude the file. No fancy path
;;; checking. That has presumably been taken care of. This handler only
;;; takes care of GET and HEAD methods.

(define (file-serve-or-dir-response fname file-path req directory-serve-response options)
  (if (file-name-directory? fname)	; Simple index generation.
      (directory-serve-response fname file-path req options)
      
      (let ((request-method (request-method req)))
	(cond
	 ((or (string=? request-method "GET")
	      (string=? request-method "HEAD")) ; Absolutely.
	  (let ((info  (stat-carefully fname req)))
	    (case (file-info:type info)
	      
	      ((regular fifo socket)
	       (send-file-response fname info req options))
	      
	      ((directory)		; Send back a redirection "foo" -> "foo/"
	       (make-error-response
		(status-code moved-perm) req
		(string-append (request-uri req) "/")
		(string-append (http-url->string (request-url req))
			       "/")))

	      (else (make-error-response (status-code forbidden) req)))))
	 
	 (else 
	  (make-error-response (status-code method-not-allowed) req 
			       request-method))))))

(define (directory-index-serve-response fname file-path req options)
  (file-serve-response (string-append fname "index.html") file-path req options))

(define (file-serve-response fname file-path req options)
  (file-serve-or-dir-response fname file-path req
			      directory-index-serve-response
			      options))

(define (default-file-name->icon-url fname)
  #f)

(define (time->directory-index-date-string time)
  (format-date "~d-~b-~y ~H:~M:~S GMT" (date time 0)))

(define (read-max-lines fname max)
  (call-with-input-file
      fname
    (lambda (port)
      (let loop ((r "") (i max))
	(if (zero? i)
	    r
	    (let ((line (read-line port)))
	      (if (eof-object? line)
		  r
		  (loop (string-append r " " line) (- i 1)))))))))

(define (string-cut s n)
  (if (>= (string-length s) n)
      (substring s 0 n)
      s))

(define html-file-header
  (let ((title-tag-regexp (make-regexp "<[Tt][Ii][Tt][Ll][Ee]>"))
	(title-close-tag-regexp (make-regexp "</[Tt][Ii][Tt][Ll][Ee]>")))
    (lambda (fname n)
      (let ((stuff (read-max-lines fname 10)))
	(cond
	 ((regexp-exec title-tag-regexp stuff)
	  => (lambda (open-match)
	       (cond
		((regexp-exec title-close-tag-regexp stuff
			      (match:end open-match 0))
		 => (lambda (close-match)
		      (string-cut (substring stuff
					     (match:end open-match 0)
					     (match:start close-match 0))
				  n)))
		(else (string-cut (substring stuff
					     (match:end open-match 0)
					     (string-length stuff))
				  n)))))
	 (else ""))))))

(define (file-documentation fname n options)
  (cond
   (((file-directory-options-file-name->content-type options) fname)
    => (lambda (content-type)
	 (if (and (string=? content-type "text/html" )
		  (file-readable? fname))
	     (html-file-header fname n)
	     "")))
   (else "")))

(define (directory-index req dir port options)
  
  (define (pad-file-name file)
    (write-string (make-string (max (- 21 (string-length file))
				    1)
			       #\space)
		  port))
  
  (define (emit-file-name file)
    (let ((l (string-length file)))
      (if (<= l 20)
	  (emit-text file port)
	  (emit-text (substring file 0 20) port))))
  
  (define (index-entry file)
    (let* ((fname (directory-as-file-name (string-append dir file)))
	   (info (file-info fname #t))
	   (type (file-info:type info))
	   (size (file-info:size info))
	   (icon-name
	    (case type
	      ((regular fifo socket)
	       ((file-directory-options-file-name->icon-url options)
		fname))
	      ((directory)
	       (file-directory-options-directory-icon-url options))
	      (else
	       (file-directory-options-unknown-icon-url options))))
	   (tag-name
	    (case type
	      ((regular fifo socket) "[FILE]")
	      ((directory) "[DIR ]")
	      (else "[????]"))))
      (if icon-name
	  (emit-tag port 'img
		    (cons 'src icon-name)
		    (cons 'alt tag-name))
	  (display tag-name port))
      (with-tag port a ((href file))
	(emit-file-name file))
      (pad-file-name file)
      (emit-text (time->directory-index-date-string (file-info:mtime info)) port)
      (if size
	  (let* ((size-string
		  (string-append (number->string (quotient size 1024))
				 "K"))
		 (size-string
		  (if (<= (string-length size-string) 7)
		      size-string
		      (string-append (number->string (quotient size (* 1024 1024)))
				     "M")))
		 (size-string
		  (if (<= (string-length size-string) 8)
		      (string-append
		       (make-string (- 8 (string-length size-string)) #\space)
		       size-string)
		      size-string)))
	    (write-string size-string port))
	  (write-string (make-string 8 #\space) port))
      (write-char #\space port)
      (emit-text (file-documentation fname 24 options) port)
      (write-crlf port)))

  (let ((files (directory-files dir)))
    (for-each index-entry files)
    (length files)))

(define (directory-serve-response fname file-path req options)
  (let ((request-method (request-method req)))
    (cond
     ((or (string=? request-method "GET")
	  (string=? request-method "HEAD"))
	  
      (if (not (eq? 'directory 
		    (file-info:type (file-info fname #t))))
	  (make-error-response (status-code forbidden) req)
	  (make-response
	   (status-code ok)
	   #f
	   (time)
	   "text/html"
	   '()
	   (make-writer-body
	    (lambda (port httpd-options)
	      (let ((back-icon
		     (file-directory-options-back-icon-url options))
		    (blank-icon
		     (file-directory-options-blank-icon-url options)))
		(with-tag port html ()
		  (let ((title (string-append "Index of /"
					      (string-join file-path "/"))))
		    (with-tag port head ()
		      (emit-title port title))
		    (with-tag port body ()
		      (emit-header port 1 title)
		      (with-tag port pre ()
			(if blank-icon
			    (display "[    ]" port)
			    (emit-tag port 'img
				      (cons 'src blank-icon)
				      (cons 'alt "      ")))
			(write-string "Name                 " port)
			(write-string "Last modified          " port)
			(write-string "Size   " port)
			(write-string "Description" port)
			(emit-tag port 'hr)
			(if back-icon
			    (emit-tag port 'img
				      (cons 'src back-icon)
				      (cons 'alt "[UP  ]"))
			    (display "[UP  ]" port))
			(if (not (null? file-path))
			    (begin
			      (with-tag port a ((href ".."))
				(write-string "Parent directory" port))
			      (write-crlf port)))
			(let ((n-files (directory-index req fname port options)))
			  (emit-tag port 'hr)
			  (format port "~d files" n-files))))))))))))
     (else
      (make-error-response (status-code method-not-allowed) req 
			   request-method)))))

(define (index-or-directory-serve-response fname file-path req options)
  (let ((index-fname (string-append fname "index.html")))
    (if (file-readable? index-fname)
	(file-serve-response index-fname file-path req options)
	(directory-serve-response fname file-path req options))))
  
(define (file-serve-and-dir-response fname file-path req options)
  (file-serve-or-dir-response fname file-path req
			      index-or-directory-serve-response
			      options))

;;; Look up user's home directory, generating an HTTP error response if you lose.

(define (http-homedir username req)
  (with-fatal-error-handler (lambda (c decline)
			      (apply http-error (status-code bad-request) req
				     "Couldn't find user's home directory."
				     (condition-stuff c)))
    (home-dir username)))


(define (send-file-response filename info req options)
  (if (file-not-readable? filename)	; #### double stats are no good
      (make-error-response (status-code not-found) req)
      (receive (stripped-filename content-encoding)
	  ((file-directory-options-file-name->content-encoding options) filename)
	(make-response (status-code ok)
		       #f
		       (time)
		       ((file-directory-options-file-name->content-type options)
			stripped-filename)
		       (append (if content-encoding
				   (list (cons 'content-encoding content-encoding))
				   '())
			       (list
				(cons 'last-modified
				      (rfc822-time->string
				       (file-info:mtime info)))
				(cons 'content-length (file-info:size info))))
		       (make-writer-body
			(lambda (port options)
			  (call-with-input-file filename
			    (lambda (in)
			      (copy-inport->outport in port)))))))))


(define (default-file-name->content-type fname)
  (let ((ext (file-name-extension fname)))
    (cond
     ((string-ci=? ext ".htm")		"text/html")
     ((string-ci=? ext ".html")		"text/html")
     ((string-ci=? ext ".txt")       	"text/plain")
     ((string-ci=? ext ".css")          "text/css")
     ((string-ci=? ext ".doc")       	"application/msword")
     ((string-ci=? ext ".gif")		"image/gif")
     ((string-ci=? ext ".png")		"image/png")
     ((string-ci=? ext ".bmp")		"image/bmp")
     ((or (string-ci=? ext ".jpg")
	  (string-ci=? ext ".jpeg"))	"image/jpeg")
     ((or (string-ci=? ext ".tiff")
	  (string-ci=? ext ".tif"))	"image/tif")
     ((string-ci=? ext ".rtf")		"text/rtf")
     ((or (string-ci=? ext ".mpeg")
	  (string-ci=? ext ".mpg"))	"video/mpeg")
     ((or (string-ci=? ext ".au")
	  (string-ci=? ext ".snd"))	"audio/basic")
     ((string-ci=? ext ".wav")		"audio/x-wav")
     ((string-ci=? ext ".dvi")		"application/x-dvi")
     ((or (string-ci=? ext ".tex")
	  (string-ci=? ext ".latex"))	"application/latex")
     ((string-ci=? ext ".zip")		"application/zip")
     ((string-ci=? ext ".tar")		"application/tar")
     ((string-ci=? ext ".hqx")		"application/mac-binhex40")
     ((string-ci=? ext ".ps") 		"application/postscript")
     ((string-ci=? ext ".pdf")		"application/pdf")
     (else               		"application/octet-stream"))))

(define (default-file-name->content-encoding fname)
  (cond
   ((let ((ext (file-name-extension fname)))
      (cond 
       ((string-ci=? ext ".Z")           "x-compress")
       ((string-ci=? ext ".gz")          "x-gzip")
       (else #f)))
    => (lambda (encoding)
	 (values (file-name-sans-extension fname) encoding)))
   (else (values fname #f))))
   
