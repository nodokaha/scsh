;;; ftp.scm -- an FTP client library for the Scheme Shell

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998 by Eric Marsden.
;;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;; The following rfc959 commands are not implemented:
;;
;; * ACCT (account; this is ignored by most servers)
;; * SMNT (structure mount, for mounting another filesystem)
;; * REIN (reinitialize connection)
;; * LOGOUT (quit without interrupting ongoing transfers)
;; * STRU (file structure)
;; * ALLO (allocate space on server)


;;; Related work ======================================================
;;
;; * rfc959 describes the FTP protocol; see
;;   http://www.ietf.org/rfc/rfc959.txt
;;
;; * /anonymous@sunsite.unc.edu:/pub/Linux/libs/ftplib.tar.gz is a
;;   library similar to this one, written in C, by Thomas Pfau
;;
;; * FTP.pm is a Perl module with similar functionality (available
;;   from http://www.perl.com/CPAN)
;;
;; * XEmacs gets transparent remote file access from EFS.
;;   However, it cheats by using /usr/bin/ftp.
;;
;; * Siod (a small-footprint Scheme implementation by George Carette)
;;   comes with a file ftp.scm with a small subset of these functions
;;   defined


;;; TODO ============================================================
;;
;; * Unix-specific commands such as SITE UMASK, SITE CHMOD
;; * improved error handling

;; Communication is initiated by the client. The server responds to
;; each request with a three digit status code and an explanatory
;; message, and occasionally with data (which is sent via a separate,
;; one-off channel). The client starts by opening a command connection
;; to a well known port on the server machine. Messages send to the
;; server are of the form
;;
;;          CMD [ <space> arg ] <CR> <LF>
;;
;; Replies from the server are of the form
;;
;;          xyz <space> Informative message <CR> <LF>
;;
;; where xyz is a three digit code which indicates whether the
;; operation succeeded or not, whether the server is waiting for more
;; data, etc. The server may also send multiline messages of the form
;;
;;          xyz- <space> Start of multiline message <CR> <LF>
;;          [ <space>+ More information ]* <CR> <LF>
;;          xyz <space> End of multiline message <CR> <LF>
;;
;; Some of the procedures in this module extract useful information
;; from the server's reply, such as the size of a file, or the name of
;; the directory we have moved to. These procedures return either the
;; extracted information, or #f to indicate failure. Other procedures
;; return a "status", which is either the server's reply as a string,
;; or #f to signify failure.

;; beware, the log file contains password information!

(define (ftp-connect host login password passive? . args)
  (let-optionals* args ((log #f))
    (let* ((hst-info (host-info host))
	   (hostname (host-info:name hst-info))
	   (srvc-info (service-info "ftp" "tcp"))
	   (sock (socket-connect protocol-family/internet
				 socket-type/stream
				 hostname
				 (service-info:port srvc-info)))
	   (connection (make-ftp-connection hostname
					    sock
					    passive?
					    log)))
      (ftp-log connection
	       (string-append "-- "
			      (date->string (date))
			      ": opened ftp connection to "
			      hostname))
      (ftp-read-reply connection (exactly-code "220")) ; the initial welcome banner
      (ftp-login connection login password)
      connection)))

;; Send user information to the remote host. Args are login
;; and password. If they are not provided, the Netrc module is used to
;; try to determine a login and password for the server.

(define (ftp-login connection login password)
  (let* ((netrc-record #f)
	 (get-netrc-record
	  (lambda ()
	    (cond
	     (netrc-record)
	     (else
	      (set! netrc-record
		    (netrc-machine-entry (ftp-connection-host-name connection) #t))
	      netrc-record)))))
    (let ((login (or login
		     (netrc-entry-login (get-netrc-record)))))
      (let ((reply
	     (ftp-send-command connection (build-command "USER" login)
			       (lambda (code)
				 (or (string=? code "331") ; "User name okay, need password."
				     (string=? code "230")))))) ; "User logged in, proceed."
					     
	(if (string-prefix? "331" reply) ; "User name okay, need password."
	    (ftp-send-command connection
			      (build-command
			       "PASS"
			       (or password
				   (netrc-entry-password (get-netrc-record))))
			      (exactly-code "230")))))))

(define-enumerated-type ftp-type :ftp-type
  ftp-type?
  ftp-types
  ftp-type-name
  ftp-type-index
  (binary ascii))

(define (ftp-set-type! connection type)
  (let ((ttype (cond
		((eq? type (ftp-type binary)) "I")
		((eq? type (ftp-type ascii)) "A"))))
    (ftp-send-command connection (build-command "TYPE" ttype))
    (values)))

(define (ftp-rename connection oldname newname)
  (ftp-send-command connection (build-command "RNFR " oldname)
		    (code-with-prefix "35"))
  (ftp-send-command connection (build-command "RNTO" newname)
		    (code-with-prefix "25"))
  (values))

(define (ftp-delete connection file)
  (ftp-send-command connection (build-command "DELE" file)
		    (code-with-prefix "25"))
  (values))

;;: connection x string -> status
(define (ftp-cd connection dir)
  (ftp-send-command connection (build-command "CWD" dir))
  (values))

;;: connection -> status
(define (ftp-cdup connection)
  (ftp-send-command connection "CDUP" (exactly-code "250"))
  (values))

;;: on success return the new directory as a string
(define (ftp-pwd connection)
  (let ((reply (ftp-send-command connection "PWD" (exactly-code "257"))))
    (cond
     ((regexp-search (rx (seq bos (= 3 digit) #\space
			      (* (~ #\")) #\" (submatch (* (~ #\"))) #\"))
		     reply)
      => (lambda (match)
	   (match:substring match 1))))))

(define (ftp-rmdir connection dir)
  (ftp-send-command connection (build-command "RMD " dir))
  (values))

(define (ftp-mkdir connection dir)
  (ftp-send-command connection (build-command "MKD ~a" dir))
  (values))

;; On success return a Scsh date record. This message is not part of
;; rfc959 but seems to be supported by many ftp servers (it's useful
;; for mirroring)

(define (ftp-modification-time connection file)
  (let* ((reply (ftp-send-command connection
				  (build-command "MDTM" file)))
         (timestr (substring reply 4 (string-length reply))))
    (let ((year  (substring timestr 0 4))
	  (month (substring timestr 4 6))
	  (mday  (substring timestr 6 8))
	  (hour  (substring timestr 8 10))
	  (min   (substring timestr 10 12))
	  (sec   (substring timestr 12 14)))
      (make-date (string->number sec)
		 (string->number min)
		 (string->number hour)
		 (string->number mday)
		 (string->number month)
		 (- (string->number year) 1900)))))

;; On success return the size of the file in bytes.
;;: connection x string -> integer
(define (ftp-size connection file)
  (let* ((reply (ftp-send-command connection
				  (build-command "SIZE" file))))
    (string->number (substring reply
			       4 (string-length reply)))))

;; Abort the current data transfer. Maybe we should close the data
;; socket?

(define (ftp-abort connection)
  (ftp-send-command connection "ABOR")
  (values))

(define (ftp-quit connection)
  (ftp-send-command connection "QUIT" (exactly-code "221"))
  (close-socket (ftp-connection-command-socket connection)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following commands require the use of a data connection as well
;; as the command connection. The command and the server's reply are
;; transmitted via the command connection, while the data is
;; transmitted via the data connection (you could have guessed that,
;; right?).
;;
;; The data socket is created by the client, who sends a PORT command
;; to the server to indicate on which port it is ready to accept a
;; connection. The port command specifies an IP number and a port
;; number, in the form of 4+2 comma-separated bytes. The server then
;; initiates the data transfer. A fresh data connection is created for
;; each data transfer (unlike the command connection which stays open
;; during the entire conversation with the server).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ftp-ls connection . maybe-dir)
  (with-data-connection
   connection
   (lambda ()
     (ftp-send-command connection
		       (apply build-command "NLST" maybe-dir)
		       (code-with-prefix "1")))
   (lambda (data-socket)
     (port->lines (socket:inport data-socket)))))

(define (find-port-arg string)
  (cond
   ((regexp-search (rx (: (+ digit) (= 5 (: #\, (+ digit))))) string)
    => (lambda (match)
	 (match:substring match 0)))))

(define (ftp-dir connection . maybe-dir)
  (with-data-connection
   connection
   (lambda ()
     (ftp-send-command connection
		       (apply build-command "LIST" maybe-dir)
		       (code-with-prefix "1")))
   (lambda (data-socket)
     (port->lines (socket:inport data-socket)))))

(define (port->lines port)
  (let loop ((reverse-lines '()))
    (let ((line (read-crlf-line port)))
      (if (eof-object? line)
	  (reverse reverse-lines)
	  (loop (cons line reverse-lines))))))

(define (ftp-get connection remote-file act)
  (with-data-connection
   connection
   (lambda ()
     (ftp-send-command connection
		       (build-command "RETR" remote-file)
		       (exactly-code "150")))
   (lambda (data-socket)
     (act (socket:inport data-socket)))))

;; FIXME: should have an optional argument :rename which defaults to
;; false, which would make us upload to a temporary name and rename at
;; the end of the upload. This atomicity is important for ftp or http
;; servers which are serving a load, and to avoid problems with "no
;; space on device".

(define (ftp-put connection remote-file act)
  (with-data-connection
   connection
   (lambda ()
     (ftp-send-command connection (build-command "STOR" remote-file)
		       (exactly-code "150")))
   (lambda (data-socket)
     (act (socket:outport data-socket)))))

(define (ftp-append connection remote-file act)
  (with-data-connection
   connection
   (lambda ()
     (ftp-send-command connection (build-command "APPE" remote-file)
		       (exactly-code "150")))
   (lambda (data-socket)
     (act (socket:outport data-socket)))))

;; send a command verbatim to the remote server and wait for a
;; reply.

(define (ftp-quot connection cmd)
  (ftp-send-command connection cmd))

;; ------------------------------------------------------------------------
;; no exported procedures below

(define (with-data-connection connection command-thunk proc)
  (if (ftp-connection-passive-mode? connection)
      (let* ((pasv-reply (ftp-send-command connection "PASV" (exactly-code "227")))
	     (port-arg (find-port-arg pasv-reply)))
	(call-with-values
	 (lambda () (parse-port-arg port-arg))
	 (lambda (address port)
	   (let ((data-socket (create-socket protocol-family/internet
					     socket-type/stream)))
	     (set-socket-option data-socket level/socket socket/reuse-address #t)
	     (connect-socket data-socket
			     (internet-address->socket-address
			      address port))
	     (command-thunk)
	     (let ((retval (proc data-socket)))
	       (close-socket data-socket)
	       (ftp-read-reply connection)
	       retval)))))

      (let* ((sock (create-socket protocol-family/internet
				  socket-type/stream))
	     (sockaddr (internet-address->socket-address
			internet-address/any
			0)))                ; 0 to accept any port
	(set-socket-option sock level/socket socket/reuse-address #t)
	(set-socket-option sock level/socket socket/linger 120)
	(bind-socket sock sockaddr)
	(listen-socket sock 0)
	(ftp-send-command connection        ; send PORT command
			  (ftp-build-PORT-string (socket-local-address sock)))
	(command-thunk)
	(receive (data-socket data-socket-address)
	    (accept-connection sock)
	  (let ((retval (proc data-socket)))
	    (close-socket data-socket)
	    (close-socket sock)
	    (ftp-read-reply connection)
	    retval)))))

;; TODO: Unix-specific commands
;; SITE UMASK 002
;; SITE IDLE 60
;; SITE CHMOD 755 filename
;; SITE HELP



;; We cache the login and password to be able to relogin automatically
;; if we lose the connection (a la ange-ftp). Not implemented.
(define-record-type ftp-connection :ftp-connection
  (make-ftp-connection host-name command-socket passive-mode? logfd)
  ftp-connection?
  (host-name ftp-connection-host-name)
  (command-socket ftp-connection-command-socket)
  (passive-mode? ftp-connection-passive-mode?)
  (logfd ftp-connection-logfd))

(define-condition-type 'ftp-error '(error))
(define ftp-error? (condition-predicate 'ftp-error))


(define (ftp-build-PORT-string sockaddr)
  (let* ((hst-info (host-info (system-name)))
         (ip-address (car (host-info:addresses hst-info))))
    (receive (hst-address srvc-port)
	(socket-address->internet-address sockaddr)
      (string-append "PORT "
		     (format-internet-host-address ip-address ",")
		     ","
		     (format-port srvc-port)))))

(define (ftp-send-command connection command . maybe-expected)
  (let* ((sock (ftp-connection-command-socket connection))
	 (out (socket:outport sock)))
    (write-string command out)
    (write-crlf out)
    (ftp-log connection (string-append "<- " command))
    (apply ftp-read-reply connection maybe-expected)))

(define any-code (lambda (code) #t))
(define (code-with-prefix prefix)
  (lambda (code)
    (string-prefix? prefix code)))
(define (exactly-code the-code)
  (lambda (code)
    (string=? code the-code)))

;; This is where we check that the server's 3 digit status code
;; corresponds to what we expected.

;; EXPECTED? is a predicate on reply codes.  If the server's reply
;; doesn't satisfy EXPECTED?, we raise an FTP-ERROR.

(define (ftp-read-reply connection . maybe-expected)
  (let-optionals* maybe-expected ((expected? (code-with-prefix "2")))
    (let* ((sock (ftp-connection-command-socket connection))
	   (in (socket:inport sock))
	   (reply (read-crlf-line in))
	   (code (substring reply 0 3)))
      (ftp-log connection (string-append "-> " reply))
      (if (not (expected? code))
	  (signal 'ftp-error reply))
      ;; handle multi-line replies
      (if (char=? (string-ref reply 3) #\-)
	  (let ((end-prefix (string-append code " ")))
	    (let loop ()
	      (let* ((line (read-crlf-line in))
		     (reply (string-join (list reply line "\n"))))
		(ftp-log connection (string-append "-> " line))
		(if (string-prefix? end-prefix line)
		    reply
		    (loop)))))
	  reply))))

(define (build-command str . opt-args)
  (string-join (cons str opt-args)))

(define (ftp-log connection line)
  (cond
   ((ftp-connection-logfd connection)
    => (lambda (log)
         (write-string line log)
         (write-string "\n" log)
         (force-output log)))))
