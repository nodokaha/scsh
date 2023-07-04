; ls clone in scsh

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998 Michael Sperber.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

; This currently does a whole bunch of stats on every file in some
; cases.  In a decent OS implementation, this stuff is cached, so
; there isn't any problem, at least not in theory :-)

; FLAGS is a list of symbols from:
;
; all        - include stuff starting with "."
; recursive  - guess what
; long       - output interesting information per file
; directory  - display only the information for the directory named
; flag       - flag files as per their types
; columns    - sorts output vertically in a multicolumn format

(define ls-crlf? (make-fluid #f))

(define (ls flags paths . maybe-port)
  (let* ((port (optional maybe-port (current-output-port)))
	 (paths (if (null? paths)
		    (list (cwd))
		    paths))
	 (only-one? (null? (cdr paths))))
    (call-with-values
     (lambda () (parse-flags flags))
     (lambda (all? recursive? long? directory? flag? columns?)
       (real-ls paths
		(if only-one? #f "")
		all? recursive? long? directory? flag? columns?
		port)))))

(define (parse-flags flags)
  (let ((all? (memq 'all flags))
	(recursive? (memq 'recursive flags))
	(long? (memq 'long flags))
	(directory? (memq 'directory flags))
	(flag? (memq 'flag flags))
	(columns? (memq 'columns flags)))
    (values all? recursive? long? directory? flag? columns?)))

(define (real-ls paths prefix
		 all? recursive? long? directory? flag? columns?
		 port)
  (let ((first #t))
    (for-each
     (lambda (path)
       (if first
	   (set! first #f)
	   (ls-newline port))
       (if prefix
	   (format port "~A~A:~%" prefix path))
       (ls-path path all? recursive? long? directory? flag? columns? port))
     paths)))

(define (ls-path path all? recursive? long? directory? flag? columns? port)
  (cond
   ((and (not directory?)                          ;; go into directories
	 (or (and (file-name-directory? path)      ;; path specifies directory
		  (file-directory? path #t))       ;; either as a symlink (if the names end with a slash)
	     (file-directory? path #f)))           ;; or not
    (ls-directory path all? recursive? long? directory? flag? columns? port))
   (else
    (if (or long? flag?)                           ;; see LS-DIRECTORY for details
	(ls-file (cons path (file-info path #f)) long? flag? port)
	(ls-file (cons path #f) long? flag? port)))))

(define (ls-directory directory all? recursive? long? directory? flag? columns? port)
; terminology: a FILE-NAME is the name of a file
;              a FILE is a pair whose car is a file-name and whose cdr is 
;                either its file-info-object or #f (if not needed)
;              a INFO is a file-info-object

  ;; This is actually a hack for the benefits of systems using AFS,
  ;; mainly to enable anonymous-upload directories: We don't list
  ;; a directory unless the Unix permissions would allow us to.
  (if (not (file-readable? directory))
      (errno-error errno/perm directory-files directory))

  (let* ((directory (file-name-as-directory directory))
	 (substantial-directory (string-append directory "."))
	 (file-names (directory-files substantial-directory all?)))
    (with-cwd*
     substantial-directory
     (lambda ()
       (let ((files (if (or recursive? long? flag?)    ; these are the flags for which we need the file-info
			(map (lambda (file-name)
			       (cons file-name (file-info file-name #f)))
			     file-names)
			(map (lambda (file-name) (cons file-name #f))
			     file-names))))

	 (if (and (not long?)
		  columns?)
	     (ls-files-columns files flag? port)
	     (ls-files-column files long? flag? port))
       
	 (if recursive?
	     (let ((directories
		    (map (lambda (file) (car file))
			 (filter (lambda (file) 
				   (eq? (file-info:type (cdr file)) 'directory))
				 files))))
	       (if (not (null? directories))
		   (begin
		     (ls-newline port)
		     (real-ls directories directory
			      all? recursive? long? directory? flag? columns?
			      port))))))))))

(define *width* 79)

(define (ls-files-columns files flag? port)
  (let* ((max-file-name-width
	  (if (null? files)
	      0
	      (apply max (map (lambda (file) (string-length (car file))) files))))
	 (max-file-name-width
	  (if flag?
	      (+ 1 max-file-name-width)
	      max-file-name-width))

	 (column-width (+ 2 max-file-name-width))

	 (columns (quotient *width*
			    column-width))
	 (columns (if (zero? columns)
		      1
		      columns))

	 (number-of-files (length files))
	 (rows (quotient (+ number-of-files (- columns 1))
			 columns))

	 (tails
	  (do ((column 0 (+ 1 column))
	       (tails (make-vector columns)))
	      ((= column columns)
	       tails)
	    (vector-set! tails column
			 (list-tail-or-null files (* rows column))))))

    (do ((row 0 (+ 1 row)))
	((= row rows))
      (do ((column 0 (+ 1 column)))
	  ((= column columns))
	(let ((tail (vector-ref tails column)))
	  (if (not (null? tail))
	      (let* ((file (car tail))
		     (width (display-file file flag? port)))
		(display-spaces (- column-width width) port)
		(vector-set! tails column (cdr tail))))))
      (ls-newline port))))

(define (list-tail-or-null list index)
  (let loop ((list list) (index index))
    (cond
     ((null? list) list)
     ((zero? index) list)
     (else (loop (cdr list) (- index 1))))))

(define (ls-files-column files long? flag? port)
  (for-each
   (lambda (file)
     (ls-file file long? flag? port))
   files))

(define (ls-file file long? flag? port)
  (if long?
      (ls-file-long file flag? port)
      (ls-file-short file flag? port)))

(define (ls-file-short file flag? port)
  (display-file file flag? port)
  (ls-newline port))

(define (ls-file-long file flag? port)
  (let ((info (cdr file)))
    (display-permissions info port)
    (display-decimal-justified (file-info:nlinks info) 4 port)
    (write-char #\space port)
    (let* ((uid (file-info:uid info))
	   (user-name
	    (call-with-current-continuation
	     (lambda (escape)
	       (with-handler
		(lambda (condition more)
		  (escape (number->string uid)))
		(lambda ()
		  (user-info:name (user-info uid))))))))
      (display-padded user-name 9 port))
    (let* ((gid (file-info:gid info))
	   (group-name
	    (call-with-current-continuation
	     (lambda (escape)
	       (with-handler
		(lambda (condition more)
		  (escape (number->string gid)))
		(lambda ()
		  (group-info:name (group-info gid))))))))
      (display-padded group-name 9 port))
    (display-decimal-justified (file-info:size info) 7 port)
    (write-char #\space port)
    (display-time  (file-info:mtime info) port)
    (write-char #\space port)
    (display-file file flag? port)
    (if (eq? (file-info:type info) 'symlink)
	(begin
	  (display " -> " port)
	  (display (read-symlink (car file)) port)))
    (ls-newline port)))

(define *year-seconds* (* 365 24 60 60))

(define (display-time the-time port)
  (let ((time-difference (abs (- (time) the-time)))
	(date (date the-time 0)))
    (if (< time-difference *year-seconds*)
	(display (format-date "~b ~d ~H:~M" date) port)
	(display (format-date "~b ~d ~Y " date) port))))

(define (display-file file flag? port)
  (let ((file-name (car file)))
    (display file-name port)
    (if (maybe-display-flag (cdr file) flag? port)
	(+ 1 (string-length file-name))
	(string-length file-name))))

(define (maybe-display-flag info flag? port)
  (and flag?
      (begin
	(cond
	 ((eq? (file-info:type info) 'directory)
	  (write-char #\/ port))
	 ((eq? (file-info:type info) 'symlink)	
	  (write-char #\@ port))
	 ; 'executable: bits 0, 3 or 6 are set:
	 ; that means, 'AND' with 1+8+64=73 results in a nonzero-value
	 ; note: there is no distinction between user's, group's and other's permissions
	 ; (as the real GNU-ls does not)
	 ((not (zero? (bitwise-and (file-info:mode info) 73)))
	  (write-char #\* port))
	 ((eq? (file-info:type info) 'socket)	
	  (write-char #\= port))
	 ((eq? (file-info:type info) 'fifo)	
	  (write-char #\| port)))
	#t)))

(define (display-permissions info port)
  (case (file-info:type info)
    ((directory)
     (write-char #\d port))
    ((symlink)
     (write-char #\l port))
    ((fifo)
     (write-char #\p port))
    (else
     (write-char #\- port)))
  (let ((mode (file-info:mode info))
	(bit 8))
    (for-each
     (lambda (id)
       (if (not (zero? (bitwise-and (arithmetic-shift 1 bit)
				    mode)))
	   (write-char id port)
	   (write-char #\- port))
       (set! bit (- bit 1)))
     '(#\r #\w #\x #\r #\w #\x #\r #\w #\x))))

(define (display-decimal-justified number width port)
  (display-justified (number->string number) width port))

(define (display-justified string width port)
  (let ((length (string-length string)))
    (if (< length width)
	(display-spaces (- width length) port))
    (display string port)))

(define (display-padded string width port)
  (let ((length (string-length string)))
    (display string port)
    (if (< length width)
	(display-spaces (- width length) port))))

(define (display-spaces number port)
  (do ((i 0 (+ 1 i)))
      ((= i number))
    (write-char #\space port)))

;; Convert Unix-style arguments to flags suitable for LS.

(define (arguments->ls-flags args)
  (let loop ((args args) (flags '()))
    (if (null? args)
	flags
	(cond
	 ((argument->ls-flags (car args))
	  => (lambda (new-flags)
	       (loop (cdr args) (append new-flags flags))))
	 (else #f)))))

(define (argument->ls-flags arg)
  (let ((arg (if (symbol? arg)
		 (symbol->string arg)
		 arg)))
    (if (or (string=? "" arg)
	    (not (char=? #\- (string-ref arg 0))))
	#f
	(let loop ((chars (cdr (string->list arg))) (flags '()))
	  (cond
	   ((null? chars)
	    flags)
	   ((char->flag (car chars))
	    => (lambda (flag)
		 (loop (cdr chars) (cons flag flags))))
	   (else #f))))))

(define (char->flag char)
  (case char
    ((#\a) 'all)
    ((#\R) 'recursive)
    ((#\l) 'long)
    ((#\d) 'directory)
    ((#\F) 'flag)
    ((#\C) 'columns)
    (else #f)))

(define (optional maybe-arg default-exp)
  (cond
   ((null? maybe-arg) default-exp)
   ((null? (cdr maybe-arg)) (car maybe-arg))
   (else (error "too many optional arguments" maybe-arg))))

(define (ls-newline port)
  (if (fluid ls-crlf?)
      (write-crlf port)
      (newline port)))