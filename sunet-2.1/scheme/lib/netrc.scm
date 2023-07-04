;;; netrc.scm -- parse authentication information contained in ~/.netrc

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (check-permissions file-name)
  (if (not (zero? (bitwise-and #b000111111 (file-mode file-name))))
      (error "Not parsing netrc file; dangerous permissions." 
	     file-name)))

(define (netrc-file-name)
  (string-append (file-name-as-directory (home-dir))
		 ".netrc"))

(define (skip-whitespace port)
  (let loop ()
    (let ((char (peek-char port)))
      (cond
       ((eof-object? char)
	(values))
       ((char-set-contains? char-set:whitespace char)
	(read-char port)
	(loop))
       (else (values))))))

(define (skip-until-eol port)
  (let loop ()
    (let ((char (peek-char port)))
      (cond
       ((eof-object? char)
	(values))
       ((char=? #\newline char)
	(read-char port))
       (else
	(read-char port)
	(loop))))))

(define (read-lines-until-double-eol port)
  (let loop ((reverse-lines '()))
    (let ((line (read-line port)))
      (if (or (eof-object? line)
	      (string=? "" line))
	  (reverse reverse-lines)
	  (loop (cons line reverse-lines))))))

(define (next-token port)
  (skip-whitespace port)
  (let loop ((reverse-chars '()))

    (define (token)
      (if (null? reverse-chars)
	  #f
	  (list->string (reverse reverse-chars))))

    (let ((char (peek-char port)))
      (cond
       ((eof-object? char) (token))
       ((char-set-contains? char-set:whitespace char) (token))
       (else
	(loop (cons (read-char port) reverse-chars)))))))

(define (next-field port)
  (let ((token (next-token port)))
    (cond
     ((not token)
      (values #f #f))
     ((string=? "default" token)
      (values token #f))
     ((string=? "macdef" token)
      (let ((name (next-token port)))
	(skip-until-eol port)
	(values token
		(cons name (read-lines-until-double-eol port)))))
     (else
      (values token (next-token port))))))

(define (skip-until-machine port machine accept-default?)
  (let loop ()
    (call-with-values
     (lambda () (next-field port))
     (lambda (tag value)
       (cond
	((not tag) #f)
	((and accept-default? (string=? "default" tag))
	 #t)
	((and (string=? tag "machine")
	      (string-ci=? machine value))
	 #t)
	(else
	 (loop)))))))

(define (next-macro-definition port)
  (let loop ()
    (call-with-values
     (lambda () (next-field port))
     (lambda (tag value)
       (cond
	((not tag) #f)
	((string=? "macdef" tag) value)
	(else (loop)))))))

(define-record-type netrc-entry :netrc-entry
  (make-netrc-entry machine login password account)
  netrc-entry?
  (machine netrc-entry-machine set-netrc-entry-machine!)
  (login netrc-entry-login set-netrc-entry-login!)
  (password netrc-entry-password set-netrc-entry-password!)
  (account netrc-entry-account set-netrc-entry-account!))

(define (netrc-machine-entry machine accept-default? . maybe-file-name)
  (let ((file-name (if (pair? maybe-file-name)
		       (car maybe-file-name)
		       (netrc-file-name)))
	(entry (make-netrc-entry machine #f #f #f)))
    (check-permissions file-name)
    (call-with-input-file file-name
      (lambda (port)
	(if (not (skip-until-machine port machine accept-default?))
	    #f
	    (let loop ()
	      (call-with-values
	       (lambda () (next-field port))
	       (lambda (tag value)
		 (cond
		  ((not tag) entry)
		  ((or (string=? "default" tag)
		       (string=? "machine" tag))
		   entry)
		  ((string=? "login" tag)
		   (set-netrc-entry-login! entry value)
		   (loop))
		  ((string=? "password" tag)
		   (set-netrc-entry-password! entry value)
		   (loop))
		  ((string=? "account" tag)
		   (set-netrc-entry-account! entry value)
		   (loop))
		  (else (loop)))))))))))

(define (netrc-macro-definitions . maybe-file-name)
  (let ((file-name (if (pair? maybe-file-name)
		       (car maybe-file-name)
		       (netrc-file-name))))
    (check-permissions file-name)
    (call-with-input-file file-name
      (lambda (port)
	(let loop ((reverse-alist '()))
	  (cond
	   ((next-macro-definition port)
	    => (lambda (pair)
		 (loop (cons pair reverse-alist))))
	   (else (reverse reverse-alist))))))))
