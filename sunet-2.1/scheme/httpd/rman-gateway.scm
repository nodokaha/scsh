;;; man page -> HTML gateway for the SU web server. -*- Scheme -*-

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1996-2003 by Mike Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; This uses RosettaMan
;;; (based at ftp.cs.berkeley.edu:/ucb/people/phelps/tcltk/rman.tar.Z)

(define (rman-handler man-binary
		      nroff-binary
		      rman-binary
		      gzcat-binary
		      finder referencer address . maybe-man)
  (let ((parse-man-url
	 (cond
	  ((procedure? finder) finder)
	  ((list? finder)
	   (lambda (url)
	     (values finder
		     (unescape-uri (http-url-search url))
		     '())))
	  (else
	   (let ((man-path 
		  ((infix-splitter ":") 
		   (or (getenv "MANPATH")
		       (begin 
			 (format (current-error-port)
				 "~%Warning: environment variable MANPATH is unset.~%")
			 "")))))
	     (lambda (url)
	       (values man-path
		       (unescape-uri (http-url-search url))
		       '()))))))
	(reference-template
	 (cond
	  ((procedure? referencer) referencer)
	  ((string? referencer) (lambda (entry section) referencer))
	  (else (lambda (entry section) "man?%s(%s)"))))
	(man (:optional maybe-man man)))

    (lambda (path req)
      (let ((request-method (request-method req)))
	(cond 
	 ((string=? request-method "GET")
	  (with-fatal-error-handler
	   (lambda (c decline)
	     (cond
	      ((http-error? c)
	       (apply http-error (car (condition-stuff c)) req
		      (cddr (condition-stuff c))))
	      (else
	       (decline))))
	   
	   (make-response
	    (status-code ok)
	    #f
	    (time)
	    "text/html"
	    '()
	    (make-writer-body
	     (lambda (out options)
	       (receive (man-path entry and-then) 
		   (parse-man-url (request-url req))
		 (emit-man-page man-binary nroff-binary rman-binary
				gzcat-binary
				entry man man-path and-then reference-template out))
	       
	       (with-tag out address ()
		 (display address out)))))))
	 (else 
	  (make-error-response (status-code method-not-allowed) req 
				    request-method)))))))

(define (cat-man-page key section out)
  (let ((title (if section
		   (format #f "~a(~a) manual page" key section)
		   (format #f "~a manual page" key))))
    (emit-title out title)
    (emit-header out 1 title)
    (newline out)
    (with-tag out body ()
      (with-tag out pre ()
	(copy-inport->outport (current-input-port)
			      out)))))

(define (emit-man-page man-binary nroff-binary rman-binary
		       gzcat-binary
		       entry man man-path and-then reference-template out)
  (receive (key section) (parse-man-entry entry)
    (let ((status
	   (cond
	    ((procedure? and-then)
	     (run (| (begin (man man-binary nroff-binary gzcat-binary
				 section key man-path))
		     (begin (and-then key section out)))
		  (= 1 ,out)
		  (= 2 ,out)))
	    (else
	     (run (| (begin (man man-binary nroff-binary gzcat-binary
				 section key man-path))
		     (,rman-binary "-fHTML"
				   ,@and-then
				   "-r" ,(reference-template entry section)))
		  (= 1 ,out)
		  (= 2 ,out))))))

      (if (not (zero? status))
	  (error "internal error emitting man page")))))
      
(define parse-man-entry
  (let ((entry-regexp (make-regexp "(.*)\\((.)\\)")))
    (lambda (s)
      (cond
       ((regexp-exec entry-regexp s)
	=> (lambda (match)
	     (values (match:substring match 1)
		     (match:substring match 2))))
       (else (values s #f))))))

(define (man man-binary nroff-binary gzcat-binary section key man-path)
  (cond
   ((procedure? man-path) (man-path))
   ((find-man-file key section "cat" man-path) =>
    (lambda (file)
      (cat-n-decode gzcat-binary file)))
   ((find-man-file key section "man" man-path) =>
    (lambda (file)
      (nroff-n-decode nroff-binary gzcat-binary file)))
   (else
    (if (not (zero?
	      (with-env (("MANPATH" . ,(string-join man-path ":")))
			(run (,man-binary "-man" ,@(if section `(,section) '()) ,key)
		     stdports))))
	(http-error (status-code not-found) #f "man page not found")))))

(define man-default-sections
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "o" "l" "n" "p"))

(define (find-man-file name section cat-man man-path . maybe-sections)

  (define (section-dir section)
    (lambda (dir)
      (file-name-as-directory
       (string-append (file-name-as-directory dir)
		      cat-man
		      section))))

  (let* ((prefix (if section
		     (string-append name "." section)
		     (string-append name ".")))
	 (pattern (string-append (glob-quote prefix) "*"))
	 (sections (:optional maybe-sections man-default-sections))
	 (path (if section
		   (map (section-dir section) man-path)
		   (apply append
			  (map (lambda (dir)
				 (map (lambda (section)
					((section-dir section) dir))
				      sections))
			       man-path)))))

    (let loop ((path path))
      (and (not (null? path))
	   (let ((matches (glob (string-append (car path) pattern))))
	     (if (not (null? matches))
		 (car matches)
		 (loop (cdr path))))))))

(define (file->man-directory file)
  (path-list->file-name
   (reverse
    (cdr
     (reverse
      (split-file-name
       (file-name-directory file)))))))

(define (cat-n-decode gzcat-binary file)
  (let ((ext (file-name-extension file)))
    (cond
     ((string=? ".gz" ext) (run (,gzcat-binary ,file) stdports))
     ((string=? ".Z" ext) (run (,gzcat-binary ,file) stdports))
     (else (call-with-input-file
	       file
	     (lambda (port)
	       (copy-inport->outport port (current-output-port))))))))

(define (nroff-n-decode nroff-binary gzcat-binary file)
  (if (not (zero? (run (| (begin (cat-n-decode gzcat-binary file))
			  (begin
			    (with-cwd (file->man-directory file)
				      (exec-epf (,nroff-binary "-man")))))
		       stdports)))
      (http-error (status-code not-found) #f "man page not found")))
