#!/bin/sh
IFS=" "
exec scsh -lm ../packages.scm -dm -o http-top -e top -s "$0" "$@"
!#

;;; Scheme Underground Web Server -*- Scheme -*-
;;; Olin Shivers

;;; To compile as a heap-image:
;;;     ,open http-top
;;;     (dump-scsh-program top "server")
;;; then insert a #! trigger.

(define-structure http-top (export top)
  (open httpd-core
	httpd-make-options
	httpd-cgi-server
	httpd-basic-handlers
	httpd-seval-handlers
	scheme-with-scsh)
  (begin

    ;; Kitchen-sink request handler.

    (define rh
      (alist-path-dispatcher
       `(("h" . ,(home-dir-handler "public_html"))
	 ("seval" . ,seval-handler)
	 ("cgi-bin" . ,(cgi-handler "/usr/local/etc/httpd/cgi-bin")))
       (tilde-home-dir-handler "public_html"
			       (rooted-file-handler "/usr/local/etc/httpd/htdocs"))))



    ;; crank up a server on port 8001, first resetting our identity to
    ;; user "nobody". Initialise the request-invariant part of the CGI
    ;; env before starting.

    (define (top args)
      (display "We be jammin, now.\n") (force-output)
      (cond ((zero? (user-uid))
	     (set-gid (->gid "nobody"))
	     (set-uid (->uid "nobody"))))
;; invariant environment is know initilialized by cgi-handler itself
;;      (initialise-request-invariant-cgi-env)
      (httpd (with-request-handler 
	      rh
	      (with-port
	       8001 
	       (with-root-directory "/usr/local/etc/httpd")))))))
