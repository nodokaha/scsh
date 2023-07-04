; RFC 959 ftp daemon

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998-2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

; It doesn't support the following desirable things:
;
; - Login by user
; - Banners from files on CWD
; - Lots of fancy stuff like ProFTPD, http://www.proftpd.org/


; following things should be improved:
;
; - GET/RETR-command: ftpd reports "Can't open FILENAME for reading" if
;   file actually doesn't exist. This is confusing. Reporting
;   "FILENAME does not exist" is much better.
; - default value for ftpd should be looked up as in ftp.scm

(define-record-type ftpd-options :ftpd-options
  (really-make-ftpd-options port anonymous-home banner
			    log-port dns-lookup?)
  ftpd-options?
  (port ftpd-options-port set-ftpd-options-port!)
  (anonymous-home ftpd-options-anonymous-home set-ftpd-options-anonymous-home!)
  (banner ftpd-options-banner set-ftpd-options-banner!)
  (log-port ftpd-options-log-port set-ftpd-options-log-port!)
  (dns-lookup? ftpd-options-dns-lookup? set-ftpd-options-dns-lookup?!))

(define (make-default-ftpd-options)
  (really-make-ftpd-options 21
			    "~ftp"
			    (list
			     (string-append "Scheme Untergrund ftp server (version "
					    sunet-version-identifier
					    ") ready."))
			    #f
			    #f))

(define (copy-ftpd-options options)
  (really-make-ftpd-options (ftpd-options-port options)
			    (ftpd-options-anonymous-home options)
			    (ftpd-options-banner options)
			    (ftpd-options-log-port options)
			    (ftpd-options-dns-lookup? options)))

(define (make-ftpd-options-transformer set-option!)
  (lambda (new-value . stuff)
    (let ((new-options (if (not (null? stuff))
			   (copy-ftpd-options (car stuff))
			   (make-default-ftpd-options))))
      (set-option! new-options new-value)
      new-options)))

(define with-port
  (make-ftpd-options-transformer set-ftpd-options-port!))
(define with-anonymous-home
  (make-ftpd-options-transformer set-ftpd-options-anonymous-home!))
(define with-banner
  (make-ftpd-options-transformer set-ftpd-options-banner!))
(define with-log-port
  (make-ftpd-options-transformer set-ftpd-options-log-port!))
(define with-dns-lookup?
  (make-ftpd-options-transformer set-ftpd-options-dns-lookup?!))

(define (make-ftpd-options . stuff)
  (let loop ((options (make-default-ftpd-options))
	     (stuff stuff))
    (if (null? stuff)
	options
	(let* ((transformer (car stuff))
	       (value (cadr stuff)))
	  (loop (transformer value options)
		(cddr stuff))))))

(define-record-type server-state :server-state
  (really-make-server-state log-lock log-port)
  server-state?
  (log-lock server-state-log-lock)
  (log-port server-state-log-port))

(define (make-server-state log-port)
  (really-make-server-state (make-lock) log-port))

(define-record-type session :session
  (really-make-session control-input-port
		       control-output-port
		       logged-in?
		       authenticated?
		       anonymous?
		       root-directory
		       current-directory
		       to-be-renamed
		       restart-position
		       replies
		       reply-code
		       type
		       data-socket
		       passive-socket)
  session?
  (control-input-port session-control-input-port
		      set-session-control-input-port!)
  (control-output-port session-control-output-port
		       set-session-control-output-port!)
  (logged-in? session-logged-in?
	      set-session-logged-in?!)
  (authenticated? session-authenticated?
		  set-session-authenticated?!)
  (anonymous? session-anonymous?
	      set-session-anonymous?!)
  (root-directory session-root-directory
		  set-session-root-directory!)
  (current-directory session-current-directory
		     set-session-current-directory!)
  (to-be-renamed session-to-be-renamed
		 set-session-to-be-renamed!)
  (restart-position session-restart-position
		    set-session-restart-position!)
  (replies session-replies
	   set-session-replies!)
  (reply-code session-reply-code
	      set-session-reply-code!)
  (type session-type
	set-session-type!)
  (data-socket session-data-socket
	       set-session-data-socket!)
  (passive-socket session-passive-socket
		  set-session-passive-socket!))

(define (make-session input-port output-port)
  (really-make-session input-port output-port
		       #f		; logged-in?
		       #f		; autenticated?
		       #f		; anonymous?
		       #f		; root-directory
		       ""		; current-directory
		       #f		; to-be-renamed
		       #f		; restart-position
		       '()		; replies
		       #f		; reply-code
		       'ascii		; type
		       #f		; data-socket
		       #f		; passive-socket
		       ))

(define session (make-fluid #f))
(define server-state (make-fluid #f))
(define options (make-fluid #f))

(define (make-session-selector selector)
  (lambda ()
    (selector (fluid session))))

(define (make-session-modifier setter)
  (lambda (value)
    (setter (fluid session) value)))

(define the-session-control-input-port
  (make-session-selector session-control-input-port))
(define the-session-control-output-port
  (make-session-selector session-control-output-port))

(define the-session-logged-in? (make-session-selector session-logged-in?))
(define the-session-authenticated? (make-session-selector session-authenticated?))
(define the-session-anonymous? (make-session-selector session-anonymous?))
(define the-session-root-directory (make-session-selector session-root-directory))
(define the-session-current-directory (make-session-selector session-current-directory))
(define the-session-to-be-renamed (make-session-selector session-to-be-renamed))
(define the-session-restart-position (make-session-selector session-restart-position))
(define the-session-replies (make-session-selector session-replies))
(define the-session-reply-code (make-session-selector session-reply-code))
(define the-session-type (make-session-selector session-type))
(define the-session-data-socket (make-session-selector session-data-socket)) 
(define the-session-passive-socket (make-session-selector session-passive-socket))

(define set-the-session-control-input-port! 
  (make-session-modifier set-session-control-input-port!))
(define set-the-session-control-output-port! 
  (make-session-modifier set-session-control-output-port!))
(define set-the-session-logged-in?!
  (make-session-modifier set-session-logged-in?!))
(define set-the-session-authenticated?!
  (make-session-modifier set-session-authenticated?!))
(define set-the-session-anonymous?!
  (make-session-modifier set-session-anonymous?!))
(define set-the-session-root-directory!
  (make-session-modifier set-session-root-directory!))
(define set-the-session-current-directory!
  (make-session-modifier set-session-current-directory!))
(define set-the-session-to-be-renamed!
  (make-session-modifier set-session-to-be-renamed!))
(define set-the-session-restart-position!
  (make-session-modifier set-session-restart-position!))
(define set-the-session-replies!
  (make-session-modifier set-session-replies!))
(define set-the-session-reply-code!
  (make-session-modifier set-session-reply-code!))
(define set-the-session-type!
  (make-session-modifier set-session-type!))
(define set-the-session-data-socket!
  (make-session-modifier set-session-data-socket!)) 
(define set-the-session-passive-socket!
  (make-session-modifier set-session-passive-socket!))

(define (make-server-state-selector selector)
  (lambda ()
    (selector (fluid server-state))))

(define (make-server-state-modifier setter)
  (lambda (value)
    (setter (fluid server-state) value)))

(define the-server-state-log-lock
  (make-server-state-selector server-state-log-lock))
(define the-server-state-log-port
  (make-server-state-selector server-state-log-port))

(define (make-ftpd-options-selector selector)
  (lambda ()
    (selector (fluid options))))

(define the-ftpd-options-port
  (make-ftpd-options-selector ftpd-options-port))
(define the-ftpd-options-anonymous-home
  (make-ftpd-options-selector ftpd-options-anonymous-home))
(define the-ftpd-options-banner
  (make-ftpd-options-selector ftpd-options-banner))
(define the-ftpd-options-log-port
  (make-ftpd-options-selector ftpd-options-log-port))
(define the-ftpd-options-dns-lookup?
  (make-ftpd-options-selector ftpd-options-dns-lookup?))

;;; LOG -------------------------------------------------------
(define (log level format-message . args)
  (syslog level 
	  (apply format #f (string-append "(thread ~D) " format-message) 
		 (thread-uid (current-thread)) args)))

(define (log-command level command-name . argument)
  (if (null? argument)
      (log level "handling ~A command" command-name)
      (if (not (null? (cdr argument)))
	  (log level "handling ~A command with argument ~S"
	       command-name argument)
	  (log level "handling ~A command with argument ~S" ; does this ever happen?
	       command-name (car argument)))))

;; Extended logging like wu.ftpd:
;; Each file up/download is protocolled

; Mon Dec  3 18:52:41 1990 1 wuarchive.wustl.edu 568881 /files.lst.Z a _ o a chris@wugate.wustl.edu ftp 0 *
;
;         %.24s %d %s %d %s %c %s %c %c %s %s %d %s
;           1   2  3  4  5  6  7  8  9  10 11 12 13
;
;         1 current time in the form DDD MMM dd hh:mm:ss YYYY
;         2 transfer time in seconds
;         3 remote host name
;         4 file size in bytes
;         5 name of file
;         6 transfer type (a>scii, b>inary)
;         7 special action flags (concatenated as needed):
;               C   file was compressed
;               U   file was uncompressed
;               T   file was tar'ed
;               _   no action taken
;         8 file was sent to user (o>utgoing) or received from
;           user (i>ncoming)
;         9 accessed anonymously (r>eal, a>nonymous, g>uest) -- mostly for FTP
;        10 local username or, if guest, ID string given
;           (anonymous FTP password)
;        11 service name ('ftp', other)
;        12 authentication method (bitmask)
;               0   none
;               1   RFC931 Authentication
;        13 authenticated user id (if available, '*' otherwise)
;
(define (file-log start-transfer-seconds info full-path direction)
  (if (the-server-state-log-port)
      (begin
	(obtain-lock (the-server-state-log-lock))
	(format (the-server-state-log-port)
		"~A ~A ~A ~A ~A ~A _ ~A a nop@ssword ftp 0 *~%" 
		(format-date "~a ~b ~d ~H:~M:~S ~Y" (date))
		(- (current-seconds) start-transfer-seconds)
		(maybe-dns-lookup
		 (socket-address->string 
		  (socket-remote-address (the-session-data-socket)) #f))
		(file-info:size info)
		(string-map (lambda (c) 
			      (if (eq? c #\space) #\_ c)) 
			    full-path)
		(case (the-session-type)
		  ((ascii) "a")
		  ((image) "b")
		  (else "?"))
		direction)
	(force-output (the-server-state-log-port))
	(release-lock (the-server-state-log-lock)))))

(define (maybe-dns-lookup ip)
  (if (the-ftpd-options-dns-lookup?)
      (or (dns-lookup-ip ip)
	  ip)
      ip))

;;; CONVERTERS ------------------------------------------------	    
(define (protocol-family->string protocol-family)
  (cond ((= protocol-family protocol-family/unspecified)
	 "unspecified")
	((= protocol-family protocol-family/internet)
	 "internet")
	((= protocol-family protocol-family/unix)
	 "unix")
	(else "unknown")))

(define (socket->string socket)
  (format #f
	  "family: ~A, ~&local address: ~A, ~&remote address: ~A, ~&input-port ~A, ~&output-port ~A"
	  (protocol-family->string (socket:family socket))
	  (socket-address->string (socket-local-address socket))
	  (socket-address->string (socket-remote-address socket))
	  (socket:inport socket)
	  (socket:outport socket)))


;;; ftpd  -------------------------------------------------------

(define (ftpd ftpd-options)
  (with-syslog-destination 
   "ftpd"
   #f
   #f
   #f
   (lambda ()
     (log (syslog-level notice) 
	  "starting daemon on port ~D with ~S as anonymous home"
	  (ftpd-options-port ftpd-options)
	  (expand-file-name (ftpd-options-anonymous-home ftpd-options)
			    (cwd)))
     (let ((the-server-state (make-server-state (ftpd-options-log-port ftpd-options))))
       (bind-listen-accept-loop
	protocol-family/internet
	(lambda (socket address)
	  (let ((remote-address (socket-address->string address)))
	    (set-ftp-socket-options! socket)
	    (fork-thread
	     (lambda ()
	       (handle-connection-encapsulated ftpd-options
					       socket
					       address
					       remote-address
					       the-server-state)))))
	(ftpd-options-port ftpd-options))))))

(define (handle-connection-encapsulated ftpd-options socket address remote-address
					the-server-state)
  (call-with-current-continuation
   (lambda (exit)
     (with-errno-handler*
      (lambda (errno packet)
	(log (syslog-level notice) 
	     "error with connection to ~A (~A)" 
	     remote-address (car packet))
	(exit 'fick-dich-ins-knie))
      (lambda ()
	(let ((socket-string (socket->string socket)))
	
	  (log (syslog-level notice)
	       "new connection to ~S"
	       remote-address)

	  (log (syslog-level debug) "socket: ~S" socket-string)
	    
	  (dynamic-wind
	   (lambda () 'fick-dich-ins-knie)
	   (lambda ()
	     (handle-connection ftpd-options
				(socket:inport socket) 
				(socket:outport socket)
				the-server-state))
	   (lambda ()
	     (log (syslog-level debug)
		  "shutting down socket ~S"
		  socket-string)
	     (call-with-current-continuation
	      (lambda (exit)
		(with-errno-handler*
		 (lambda (errno packet)
		   (log (syslog-level notice)
			"error shutting down socket to ~A (~A)"
			remote-address (car packet))
		   (exit 'fick-dich-ins-knie))
		 (lambda ()
		   (shutdown-socket socket shutdown/sends+receives)))))
	     (log (syslog-level notice)
		  "closing connection to ~A and finishing thread" remote-address)
	     (log (syslog-level debug)
		  "closing socket ~S" socket-string)
	     (close-socket socket)))))))))

(define (ftpd-inetd ftpd-options)
  (with-syslog-destination 
   "ftpd"
   #f
   #f
   #f
   (lambda ()
     (log (syslog-level notice)
	  "starting ftpd from inetd"
	  (expand-file-name (ftpd-options-anonymous-home ftpd-options)
			    (cwd)))
     (handle-connection ftpd-options
			(current-input-port)
			(current-output-port)
			(make-server-state (ftpd-options-log-port ftpd-options))))))

(define (set-ftp-socket-options! socket)
  ;; If the client closes the connection, we won't lose when we try to
  ;; close the socket by trying to flush the output buffer.
  ;; ... only it somehow exposes a bug in Windows Internet Explorer
  ;; so we leave it disabled.
  ;; (set-port-buffering (socket:outport socket) bufpol/none)

  (set-socket-option socket level/socket tcp/no-delay #t)

  (set-socket-option socket level/socket socket/oob-inline #t))


(define (handle-connection ftpd-options input-port output-port the-server-state)
  (log (syslog-level debug)
       "handling connection with input port ~A, output port ~A"
       input-port
       output-port)
  (call-with-current-continuation
   (lambda (escape)
     (with-handler
      (lambda (condition more)
	(log (syslog-level notice)
	     "hit error condition ~A (~S) -- exiting"
	     (condition-type condition)
	     (condition-stuff condition))
	(escape 'fick-dich-ins-knie))
      (lambda ()
	(let-fluids
	 session (make-session input-port output-port)
	 server-state the-server-state
	 options ftpd-options
	 (lambda ()
	   (display-banner)
	   (handle-commands))))))))

(define (display-banner)
  (log (syslog-level debug)
       "displaying banner (220)")
  (apply register-reply! 220 (the-ftpd-options-banner)))

(define-condition-type 'ftpd-quit '())
(define ftpd-quit? (condition-predicate 'ftpd-quit))

(define-condition-type 'ftpd-irregular-quit '())
(define ftpd-irregular-quit? (condition-predicate 'ftpd-irregular-quit))

(define-condition-type 'ftpd-error '())
(define ftpd-error? (condition-predicate 'ftpd-error))


(define (handle-commands)
  (log (syslog-level debug) "handling commands")
  (call-with-current-continuation
   (lambda (exit)
     (with-handler
      (lambda (condition more)
	(if (ftpd-quit? condition)
	    (begin
	      (log (syslog-level debug) "quitting (write-accept-loop)")
	      (with-handler
	       (lambda (condition ignore)
		 (more))
	       (lambda ()
		 (write-replies)
		 (exit 'fick-dich-ins-knie))))
	    (more)))
      (lambda ()
	(log (syslog-level debug)
	     "starting write-accept-loop")
	(let loop ()
	  (write-replies)
	  (accept-command)
	  (loop)))))))

(define (accept-command)
  (let* ((timeout-seconds 90)
	 (command-line (read-crlf-line-timeout (the-session-control-input-port)
					       #f
					       (* 1000 timeout-seconds);timeout
					       500)))    ; max interval
    (log (syslog-level debug) 
	 "Command line: ~A" 
	 command-line)
    (cond ((eq? command-line 'timeout)
	   (log (syslog-level notice) "hit timelimit of ~D seconds (421)"
		timeout-seconds)
	   (log (syslog-level debug)
		"so closing control connection and quitting")
	   (register-reply! 
	    421
	    (format #f "Timeout (~D seconds): closing control connection."
		    timeout-seconds)
	    (signal 'ftpd-quit)))
	   (else
	    (call-with-values
	     (lambda () (parse-command-line command-line))
	     (lambda (command arg)
	       (handle-command command arg)))))))

(define (handle-command command arg)
;;  (log (syslog-level debug)
;;       "handling command ~S with argument ~S"
;;       command arg)
  (call-with-current-continuation
   (lambda (exit)
     (with-handler
      (lambda (condition more)
	(cond
	 ((error? condition)
	  (let ((reason (condition-stuff condition)))
	    (log (syslog-level notice)
		 "internal error occurred: ~S (maybe reason: ~S) -- replying and escaping (451)"
		 condition reason)
	    (replace-reply! 451
			    (format #f "Internal error: ~S" reason))
	    (exit 'fick-dich-ins-knie)))
	 ((ftpd-error? condition)
	  ;; debug level because nearly every unsuccessful command ends
	  ;; here (no args, can't change dir, etc.)
	  (log (syslog-level debug)  
	       "ftpd error occurred (maybe reason: ~S)-- escaping" (condition-stuff condition))
	  (exit 'fick-dich-ins-knie))
	 (else
	  (more))))
      (lambda ()
	(with-errno-handler*
	 (lambda (errno packet)
	   (let ((unix-error (car packet)))
	     (log (syslog-level notice)
		  "unix error occurred: ~S -- replying (451) and escaping"
		  unix-error)
	     (replace-reply! 451
			     (format #f "Unix error: ~A." unix-error))))
	 (lambda ()
	   (dispatch-command command arg))))))))

(define (dispatch-command command arg)
;  (log (syslog-level debug)
;       "dispatching command ~S with argument ~S"
;       command arg)
  (cond
   ((assoc command *command-alist*)
    => (lambda (pair)
	 (log (syslog-level debug)
	      "command ~S was found in command-list and is executed with argument ~S"
	      (car pair) arg)
	 ((cdr pair) arg)))
   (else
    (log (syslog-level debug) "rejecting unknown command ~S (500) (argument: ~S)"
	 command arg)
    (register-reply! 500
		     (string-append
		      (format #f "Unknown command: \"~A\"" command)
		      (if (string=? "" arg)
			  "."
			  (format #f " (argument(s) \"~A\")." arg)))))))


(define (handle-user name)
  (log-command (syslog-level info) "USER" name)
  (cond
   ((the-session-logged-in?)
    (log (syslog-level info) "user ~S is already logged in (230)"
	 name)
    (register-reply! 230
		     "You are already logged in."))
   ((or (string=? "anonymous" name)
	(string=? "ftp" name))
    (handle-user-anonymous))
   (else
    (log (syslog-level info) "rejecting non-anonymous login (530)")
    (register-reply! 530
		     "Only anonymous logins allowed."))))

(define (handle-user-anonymous)
  (log (syslog-level info) "anonymous user login (230)")
  (set-the-session-logged-in?! #t)
  (set-the-session-authenticated?! #t)
  (set-the-session-anonymous?! #t)
  (set-the-session-root-directory!
   (file-name-as-directory (the-ftpd-options-anonymous-home)))
  (set-the-session-current-directory! "")
  
  (register-reply! 230 "Anonymous user logged in."))

(define (handle-pass password)
  (log-command (syslog-level info) "PASS" password)
  (cond
   ((not (the-session-logged-in?))
    (log (syslog-level info) "Rejecting password; user has not logged in yet. (530)")
    (register-reply! 530 "You have not logged in yet."))
   ((the-session-anonymous?)
    (log (syslog-level info) "Accepting password; user is logged in (200)")
    (register-reply! 200 "Thank you."))
   (else
    (log (syslog-level notice) "Reached unreachable case-branch while handling password (502)")
    (register-reply! 502 "This can't happen."))))

(define (handle-quit foo)
  (log-command (syslog-level info) "QUIT")
  (log (syslog-level debug) "quitting (221)")
  (register-reply! 221 "Goodbye!  Au revoir!  Auf Wiedersehen!")
  (signal 'ftpd-quit))

(define (handle-syst foo)
  (log-command (syslog-level info) "SYST")
  (log (syslog-level debug) "telling system type (215)")
  (register-reply! 215 "UNIX Type: L8"))

(define (handle-cwd path)
  (log-command (syslog-level info) "CWD" path)
  (ensure-authenticated-login)
  (let ((current-directory (assemble-path (the-session-current-directory)
					  path)))
    (with-errno-handler*
     (lambda (errno packet)
       (let ((error-reason (car packet)))
	 (log (syslog-level info)
	      "can't change to directory \"~A\": ~A (550)"
	      path error-reason)
	 (signal-error! 550
			(format #f "Can't change directory to \"~A\": ~A."
				path
				error-reason))))
     (lambda ()
       (with-cwd*
	(file-name-as-directory
	 (string-append (the-session-root-directory) current-directory))
	(lambda ()			; I hate gratuitous syntax
	  (log (syslog-level debug)
	       "changing current directory to \"/~A\" (250)"
	       current-directory)
	  (set-the-session-current-directory! current-directory)
	  (register-reply! 250
			   (format #f "Current directory changed to \"/~A\"."
				   current-directory))))))))

(define (handle-cdup foo)
  (log-command (syslog-level info) "CDUP")
  (handle-cwd ".."))

(define (handle-pwd foo)
  (log-command (syslog-level info) "PWD")
  (ensure-authenticated-login)
  (let ((current-directory (the-session-current-directory)))
    (log (syslog-level info) "replying \"/~A\" as current directory (257)"
	 current-directory)
    (register-reply! 257
		     (format #f "Current directory is \"/~A\"."
			     current-directory))))


(define (make-file-action-handler error-format-string action)
  (lambda (path)
    (ensure-authenticated-login)
    (if (string=? "" path)
	(begin
	  (log (syslog-level info) 
	       "finishing processing command because of missing arguments (500)")
	  (signal-error! 500 "No argument.")))
    (let ((full-path (string-append (the-session-root-directory)
				    (assemble-path (the-session-current-directory)
						   path))))
      (with-errno-handler*
       (lambda (errno packet)
	 (let ((error-reason (car packet)))
	   (log (syslog-level info)
		(string-append error-format-string " (550)") path error-reason)
	   (signal-error! 550
			  (format #f error-format-string
				  path error-reason))))
       (lambda ()
	 (action path full-path))))))

(define handle-dele
  (make-file-action-handler
   "Could not delete \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "DELE" path)
     (delete-file full-path)
     (log (syslog-level debug) "deleted ~S (250)" full-path)
     (log (syslog-level debug) "reporting about ~S" path)
     (register-reply! 250 (format #f "Deleted \"~A\"." path)))))

(define handle-mdtm
  (make-file-action-handler
   "Could not get info on \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "MDTM" path)
     (let* ((info (file-info full-path))
	    (the-date (date (file-info:mtime info) 0))
	    (formatted-date (format-date "~Y~m~d~H~M~S" the-date)))
       (log (syslog-level debug) "reporting modification time of ~S: ~A (213)"
	    full-path
	    formatted-date)
       (register-reply! 213
			formatted-date)))))

(define handle-mkd
  (make-file-action-handler
   "Could not make directory \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "MKD" path)
     (create-directory full-path #o755)
     (log (syslog-level debug) "created directory ~S (257)" full-path)
     (log (syslog-level debug) "reporting about ~S" path)
     (register-reply! 257
		      (format #f "Created directory \"~A\"." path)))))

(define handle-rmd
  (make-file-action-handler
   "Could not remove directory \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "RMD" path)
     (delete-directory full-path)
     (log (syslog-level debug) "deleted directory ~S (250)" full-path)
     (log (syslog-level debug) "reporting about ~S" path)
     (register-reply! 250
		      (format #f "Deleted directory \"~A\"." path)))))


(define handle-rnfr
  (make-file-action-handler
   "Could not get info on file \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "RNFR" path)
     (file-info full-path)
     (log (syslog-level debug) 
	  "RNFR-command accepted, waiting for RNTO-command (350)")
     (register-reply! 350 "RNFR accepted.  Gimme a RNTO next.")
     (set-the-session-to-be-renamed! full-path))))

(define (handle-rnto path)
  (log-command (syslog-level info) "RNTO" path)
  (ensure-authenticated-login)
  (if (not (the-session-to-be-renamed))
      (begin
	(log (syslog-level info) 
	     "RNTO-command rejected: need RNFR-command before (503)")
	(signal-error! 503 "Need RNFR before RNTO.")))
  (if (string=? "" path)
      (begin
	(log (syslog-level info)
	     "No argument -- still waiting for (correct) RNTO-command (500)")
	(signal-error! 500 "No argument.")))
  (let ((full-path (string-append (the-session-root-directory)
				  (assemble-path (the-session-current-directory)
						 path))))

    (if (file-exists? full-path)
	(begin
	  (log (syslog-level info) "rename of ~S failed (already exists) (550)"
	       full-path)
	  (log (syslog-level debug) "reporting about ~S" 
	       path)
	  (signal-error!
	   550
	   (format #f "Rename failed---\"~A\" already exists or is protected."
		   path))))

    (with-errno-handler*
     (lambda (errno packet)
       (log (syslog-level info)
	    "failed to rename ~A (550)" path)
       (signal-error! 550
		      (format #f "Could not rename: ~A." path)))
     (lambda ()
       (let ((old-name (the-session-to-be-renamed)))
	 (rename-file old-name full-path)
	 (log (syslog-level debug)
	      "~S renamed to ~S - no more waiting for RNTO-command (250)"
	      old-name full-path)
	 (register-reply! 250 "File renamed.")
	 (set-the-session-to-be-renamed! #f))))))
  
(define handle-size
  (make-file-action-handler
   "Could not get info on file \"~A\": ~A."
   (lambda (path full-path)
     (log-command (syslog-level info) "SIZE" path)
     (let ((info (file-info full-path)))
       (if (not (eq? 'regular (file-info:type info)))
	   (begin
	     (log (syslog-level info)
		  "rejecting SIZE-command as ~S is not a regular file (550)"
		  full-path)
	     (log (syslog-level debug) "reporting about ~S" path)
	     (signal-error! 550
			    (format #f "\"~A\" is not a regular file."
				    path))))
       (let ((file-size (file-info:size info)))
	 (log (syslog-level debug)
	      "reporting ~D as size of ~S (213)"
	      file-size full-path)
	 (register-reply! 213 (number->string file-size)))))))


(define (handle-type arg)
  (log-command (syslog-level info) "TYPE" arg)
  (cond
   ((string-ci=? "A" arg)
    (log (syslog-level debug) "changed type to ascii (200)")
    (set-the-session-type! 'ascii))
   ((string-ci=? "I" arg)
    (log (syslog-level debug) "changed type to image (8-bit binary) (200)")
    (set-the-session-type! 'image))
   ((string-ci=? "L8" arg)
    (log (syslog-level debug) "changed type to image (8-bit binary) (200)")
    (set-the-session-type! 'image))
   (else
    (log (syslog-level info)
	 "rejecting TYPE-command: unknown type (504)")
    (signal-error! 504
		   (format #f "Unknown TYPE: ~S." arg))))

  (log (syslog-level debug) "reporting new type (see above)")
  (register-reply! 200
		   (format #f "TYPE is now ~A."
			   (case (the-session-type)
			     ((ascii) "ASCII")
			     ((image) "8-bit binary")
			     (else "somethin' weird, man")))))

(define (handle-mode arg)
  (log-command (syslog-level info) "MODE" arg)
  (cond
   ((string=? "" arg)
    (log (syslog-level info) "rejecting MODE-command: no arguments (500)")
    (register-reply! 500
		     "No arguments.  Not to worry---I'd ignore them anyway."))
   ((string-ci=? "S" arg)
    (log (syslog-level info) 
	 "stream mode is (still) used for file-transfer (200)")
    (register-reply! 200 "Using stream mode to transfer files."))
   (else
    (log (syslog-level info) "mode ~S is not supported (504)" arg)
    (register-reply! 504 (format #f "Mode \"~A\" is not supported."
				 arg)))))

(define (handle-stru arg)
  (log-command (syslog-level info) "STRU" arg)
  (cond
   ((string=? "" arg)
    (log (syslog-level info) "rejecting STRU-command: no arguments (500)")
    (register-reply! 500
		     "No arguments.  Not to worry---I'd ignore them anyway."))
   ((string-ci=? "F" arg)
    (log (syslog-level debug) "(still) using file structure to transfer files (200)")
    (register-reply! 200 "Using file structure to transfer files."))
   (else
    (log (syslog-level info) "file structure ~S is not supported (504)" arg)
    (register-reply! 504
		     (format #f "File structure \"~A\" is not supported."
			     arg)))))

(define (handle-noop arg)
  (log-command (syslog-level info) "NOOP")
  (log (syslog-level debug) "successfully done nothing (200)")
  (register-reply! 200 "Done nothing, but successfully."))

(define (ftpd-parse-port-arg stuff)
  (with-fatal-error-handler*
   (lambda (condition more)
     (log (syslog-level debug) "reporting syntax error in argument (500)")
     (signal-error! 500
		    "Syntax error in argument to PORT."))
   (lambda ()
     (parse-port-arg stuff))))

(define (handle-port stuff)
  (log-command (syslog-level info) "PORT" stuff)
  (ensure-authenticated-login)
  (maybe-close-data-connection)
  (call-with-values
   (lambda () (ftpd-parse-port-arg stuff))
   (lambda (address port)
     (let ((socket (create-socket protocol-family/internet
				  socket-type/stream)))
       (log (syslog-level debug)
	    "created new socket (internet, stream, reusing address)")
       (set-socket-option socket level/socket socket/reuse-address #t)

       (connect-socket socket
		       (internet-address->socket-address
			address port))
       
       (set-the-session-data-socket! socket)

       (let ((formatted-internet-host-address 
	      (format-internet-host-address address)))
	 (log (syslog-level debug)
	      "connected to ~A, port ~A (200)" 
	      formatted-internet-host-address port)

	 (register-reply! 200
			  (format #f "Connected to ~A, port ~A."
				  formatted-internet-host-address 
				  port)))))))


(define (handle-pasv stuff)
  (log-command (syslog-level info) "PASV")
  (ensure-authenticated-login)
  (maybe-close-data-connection)
  (let ((socket (create-socket protocol-family/internet
			       socket-type/stream)))
    
    (set-socket-option socket level/socket socket/reuse-address #t)

    (bind-socket socket
		 (internet-address->socket-address (this-host-address)
						   0))
    (listen-socket socket 1)

    (let ((address (socket-local-address socket)))

      (call-with-values
       (lambda () (socket-address->internet-address address))
       (lambda (host-address port)

	 (set-the-session-passive-socket! socket)


	 (let ((formatted-this-host-address 
		(format-internet-host-address (this-host-address) ","))
	       (formatted-port (format-port port)))
	   (log (syslog-level debug) "accepting passive mode (on ~A,~A) (227)"
		formatted-this-host-address formatted-port)
	   (register-reply! 227
			    (format #f "Passive mode OK (~A,~A)"
				    formatted-this-host-address
				    formatted-port))))))))

(define (this-host-address)
  (let ((socket (port->socket (the-session-control-input-port)
			      protocol-family/internet)))
    (call-with-values
     (lambda () 
       (socket-address->internet-address 
	(socket-local-address socket)))
     (lambda (host-address control-port)
       (close-socket socket)
       host-address))))

(define (handle-nlst arg)
  (log-command (syslog-level info) "NLST" arg)
  (handle-listing arg '()))

(define (handle-list arg)
  (log-command (syslog-level info) "LIST" arg)
  (handle-listing arg '(long)))
  
(define (handle-listing arg preset-flags)
  (ensure-authenticated-login)
  (let ((args (split-arguments arg)))
    (call-with-values
     (lambda ()
       (partition
        (lambda (arg)
          (and (not (string=? "" arg))
               (char=? #\- (string-ref arg 0))))
        args))
     (lambda (flag-args rest-args)

       (if (and (not (null? rest-args))
                (not (null? (cdr rest-args))))
           (begin
             (log (syslog-level info) "got more than one path argument - rejection (501)")
             (signal-error! 501 "More than one path argument.")))

       (let ((path (if (null? rest-args)
                       ""
                       (car rest-args)))
             (flags (arguments->ls-flags flag-args)))

         (if (not flags)
             (begin
               (log (syslog-level info) "got invalid flags (501)")
               (signal-error! 501 "Invalid flag(s).")))
         (let ((all-flags (append preset-flags flags)))
           (log (syslog-level debug)
                "sending file-listing for path ~S with flags ~A"
                path all-flags)
           (generate-listing path all-flags)))))))

; Note this doesn't call ENSURE-AUTHENTICATED-LOGIN or
; ENSURE-DATA-CONNECTION.

(define (generate-listing path flags)
  (let ((full-path (string-append (the-session-root-directory)
				  (assemble-path (the-session-current-directory)
						 path))))
    (with-errno-handler*
     (lambda (errno packet)
       (let ((error-reason (car packet)))
	 (log (syslog-level info) 
	      "can't access directory at ~A: ~A (451)"
	      path error-reason)
	 (signal-error! 451
			(format #f "Can't access directory at ~A: ~A."
				path
				error-reason))))
     (lambda ()
       (with-cwd*
	(file-name-directory full-path)
	(lambda ()
          (with-data-connection
           (lambda ()
             (let ((nondir (file-name-nondirectory full-path)))
               (let-fluid
                   ls-crlf? #t
                 (lambda ()
                   (ls flags
                       (list
                        ;; work around OLIN BUG
                        (if (string=? nondir "")
                            "."
                            nondir))
                       (socket:outport (the-session-data-socket))))))))))))))

(define (handle-abor foo)
  (log-command (syslog-level info) "ABOR")
  (maybe-close-data-connection)
  (log (syslog-level debug) "closing data connection (226)")
  (register-reply! 226 "Closing data connection."))

(define (handle-rest restart-position)
  (log-command (syslog-level info) "REST" restart-position)
  (ensure-authenticated-login)
  (cond ((string->number restart-position) =>
	 (lambda (restart-position)
	   (log-command (syslog-level debug)
			"REST-command accepted, waiting for RETR or STOR (350)")
	   (register-reply!
	    350
	    (format #f "Restarting at ~A.  Gimme RETR or STOR next." restart-position))
	   (set-the-session-restart-position! restart-position)))
	(else
	 (register-reply! 501 "REST requires a value greater than or equal to 0."))))

(define (handle-retr path)
  (log-command (syslog-level info) "RETR" path)
  (ensure-authenticated-login)
  (let ((full-path (string-append (the-session-root-directory)
				  (assemble-path (the-session-current-directory)
						 path))))
    (with-fatal-error-handler*		; CALL-WITH-INPUT-FILE doesn't go through ERRNO
     (lambda (condition more)
       (let ((reason (condition-stuff condition)))
	 (log (syslog-level info) "failed to open ~S for reading (maybe reason: ~S) (550)" full-path reason)
	 (log (syslog-level debug) "replying error for file ~S (maybe reason: ~S)" path reason)
	 (signal-error! 550
			(format #f "Can't open \"~A\" for reading."
				path))))
     (lambda ()
       (let ((info (file-info full-path))
	     (start-transfer-seconds (current-seconds)))
	 (if (not (eq? 'regular (file-info:type info)))
	     (begin
	       (log (syslog-level info) "rejecting RETR-command as ~S is not a regular file (450)" 
		    full-path)
	       (log (syslog-level debug) "reporting about ~S" path)
	       (signal-error! 450
			      (format #f "\"~A\" is not a regular file."
				      path))))
	 (call-with-input-file full-path
	   (lambda (file-port)
	     (cond ((the-session-restart-position) =>
		    (lambda (restart-position)
		      (log (syslog-level debug) "clearing RESTART position")
		      (set-the-session-restart-position! #f)
                      (if (not (zero? restart-position))
                          (begin
                            ;; scsh can seek on unbuffered ports only
                            (set-port-buffering file-port bufpol/none)
                            (seek file-port restart-position)
                            (log (syslog-level debug)
                                 "seeking for RESTART at position ~A successful" 
                                 restart-position))
                          (log (syslog-level debug)
                               "Position is 0, no seek necessary")))))
	     (with-data-connection
	      (lambda ()
		(case (the-session-type)
		  ((image)
		   (log (syslog-level debug) 
			"sending file ~S (binary mode)"
			full-path)
		   (log (syslog-level debug) "sending is from port ~S" file-port)
		   (copy-port->port-binary
		    file-port
		    (socket:outport (the-session-data-socket))))
		  ((ascii)
		   (log (syslog-level debug) "sending file ~S (ascii mode)" 
			full-path)
		   (log (syslog-level debug) "sending is from port ~S" file-port)
		   (copy-port->port-ascii
		    file-port
		    (socket:outport (the-session-data-socket)))))
		(file-log start-transfer-seconds info full-path "o"))))))))))

(define (current-seconds)
  (receive (time ticks) (time+ticks) time))

; Adapted from CALL-WITH-MUMBLE-FILE in scsh/newports.scm
; Is DYNAMIC-WIND really needed for this case?
(define (call-with-output-file/flags string flags proc)
  (let ((port #f))
    (dynamic-wind (lambda ()
		    (if port
			(warn "throwing back into a call-with-output-file/flags"
			      string)
			(set! port (open-output-file string flags))))
	(lambda () (proc port))
	(lambda ()
	  (if port
	      (close port))))))

(define (handle-stor path)
  (log-command (syslog-level info) "STOR" path)
  (ensure-authenticated-login)
  (let ((full-path (string-append (the-session-root-directory)
				  (assemble-path (the-session-current-directory)
						 path))))
    (with-fatal-error-handler*
     (lambda (condition more)
       (let ((reason (condition-stuff condition)))
	 (log (syslog-level info) "can't open ~S for writing (maybe reason: ~S) (550)" full-path reason)
	 (log (syslog-level debug) "replying error for file ~S (maybe reason: ~S)" path reason)
	 (signal-error! 550 (format #f "Can't open \"~A\" for writing." path))))
     (lambda ()
       (let ((start-transfer-seconds (current-seconds)))
	 (call-with-output-file/flags full-path
				      (if (the-session-restart-position)
					  (bitwise-ior open/create)
					  (bitwise-ior open/create open/truncate))
	   (lambda (file-port)
	     (cond ((the-session-restart-position) =>
		    (lambda (restart-position)
		      (log (syslog-level debug) "clearing RESTART position")
		      (set-the-session-restart-position! #f)
                      (if (not (zero? restart-position))
                          (begin
                            ;; scsh can seek on unbuffered ports only
                            (set-port-buffering file-port bufpol/none)
                            (seek file-port restart-position)
                            (log (syslog-level debug)
                                 "seeking for RESTART at position ~A successful" 
                                 restart-position))
                          (log (syslog-level debug)
                               "Position is 0, no seek necessary")))))
	     (with-data-connection
	      (lambda ()
		(let ((inport (socket:inport (the-session-data-socket))))
		  (case (the-session-type)
		    ((image)
		     (log (syslog-level notice) 
			  "storing data to ~S (binary mode)" 
			  full-path)
		     (log (syslog-level debug)
			  "storing comes from socket-inport ~S (binary-mode)"
			  inport)
		     (copy-port->port-binary
		      (socket:inport (the-session-data-socket))
		      file-port))
		    ((ascii)
		     (log (syslog-level notice)
			  "storing data to ~S (ascii-mode)"
			  full-path)
		     (log (syslog-level debug)
			  "storing comes from socket-inport ~S (ascii-mode)"
			  inport)
		     (copy-ascii-port->port
		      (socket:inport (the-session-data-socket))
		      file-port)))
		  (file-log start-transfer-seconds (file-info full-path) full-path "i")
		  ))))))))))
  
(define (assemble-path current-directory path)
  (log (syslog-level debug) "assembling path ~S"
       path)
  (let* ((interim-path
	  (if (not (file-name-rooted? path))
	      (string-append (file-name-as-directory current-directory)
			     path)
	      path))
	 (complete-path (if (file-name-rooted? interim-path)
			    (file-name-sans-rooted interim-path)
			    interim-path)))
    (log (syslog-level debug) "name ~S assembled to ~S"
	 path complete-path)
    (cond
     ((normalize-path complete-path)
      => (lambda (assembled-path) assembled-path))
     (else
      (log (syslog-level debug) 
	   "invalid pathname -- tried to pass root directory (501)")
      (signal-error! 501 "Invalid pathname")))))

(define (ensure-authenticated-login)
  (if (or (not (the-session-logged-in?))
	  (not (the-session-authenticated?))) 
      (begin
	(log (syslog-level debug) 
	     "login authentication failed - user is not logged in (530)")
	(signal-error! 530 "You're not logged in yet."))
      (log (syslog-level debug) "authenticated login ensured")))

(define (with-data-connection thunk)
  (ensure-data-connection)
  (with-fatal-error-handler*
   (lambda (condition more)
     (maybe-close-data-connection)
     (more))
   (lambda ()
     (thunk)
     (maybe-close-data-connection)
     (log (syslog-level debug) "closing data connection (226)")
     (register-reply! 226 "Closing data connection."))))

(define *window-size* 8192)

(define (ensure-data-connection)
  (if (and (not (the-session-data-socket)) 
	   (not (the-session-passive-socket)))
      (begin 
	(log (syslog-level debug) "no data connection (425)")
	(signal-error! 425 "No data connection.")))

  (if (the-session-passive-socket)
      (call-with-values
       (lambda () (accept-connection (the-session-passive-socket)))
       (lambda (socket socket-address)
	 (set-the-session-data-socket! socket))))

  (log (syslog-level debug) "opening data connection (150)")
  (register-reply! 150 "Opening data connection.")
  (write-replies)

  (set-socket-option (the-session-data-socket) level/socket
		     socket/send-buffer *window-size*)
  (set-socket-option (the-session-data-socket) level/socket
		     socket/receive-buffer *window-size*))

(define (maybe-close-data-connection)
  (if (or (the-session-data-socket) (the-session-passive-socket))
      (close-data-connection)))

(define (close-data-connection)
  (if (the-session-data-socket)
      (close-socket (the-session-data-socket)))
  (if (the-session-passive-socket)
      (close-socket (the-session-passive-socket)))
  (set-the-session-data-socket! #f)
  (set-the-session-passive-socket! #f))

(define *command-alist*
  (list
   (cons "NOOP" handle-noop)
   (cons "USER" handle-user)
   (cons "PASS" handle-pass)
   (cons "QUIT" handle-quit)
   (cons "SYST" handle-syst)
   (cons "CWD" handle-cwd)
   (cons "PWD" handle-pwd)
   (cons "CDUP" handle-cdup)
   (cons "DELE" handle-dele)
   (cons "MDTM" handle-mdtm)
   (cons "MKD" handle-mkd)
   (cons "RMD" handle-rmd)
   (cons "RNFR" handle-rnfr)
   (cons "RNTO" handle-rnto)
   (cons "SIZE" handle-size)
   (cons "TYPE" handle-type)
   (cons "MODE" handle-mode)
   (cons "STRU" handle-stru)
   (cons "PORT" handle-port)
   (cons "PASV" handle-pasv)
   (cons "NLST" handle-nlst)
   (cons "LIST" handle-list)
   (cons "REST" handle-rest)
   (cons "RETR" handle-retr)
   (cons "STOR" handle-stor)
   (cons "ABOR" handle-abor)))

(define (parse-command-line line)
  (if (eof-object? line) ; Netscape does this
      (signal 'ftpd-irregular-quit)
      (let* ((line (string-trim-both line char-set:whitespace))
	     (split-position (string-index line #\space)))
	(if split-position
	    (values (string-map char-upcase (substring line 0 split-position))
		    (string-trim-both (substring line
						 (+ 1 split-position)
						 (string-length line))
				      char-set:whitespace))
	    (values (string-map char-upcase line) "")))))

; Path names

; This removes all internal ..'s from a path.
; NORMALIZE-PATH returns #f if PATH points to a parent directory.

(define (normalize-path path)
  (let loop ((components (split-file-name (simplify-file-name path)))
	     (reverse-result '()))
    (cond
     ((null? components)
      (path-list->file-name (reverse reverse-result)))
     ((string=? ".." (car components))
      (if (null? reverse-result)
	  #f
	  (loop (cdr components) (cdr reverse-result))))
     (else
      (loop (cdr components) (cons (car components) reverse-result))))))

(define (file-name-rooted? file-name)
  (and (not (string=? "" file-name))
       (char=? #\/ (string-ref file-name 0))))

(define (file-name-sans-rooted file-name)
  (substring file-name 1 (string-length file-name)))

(define split-arguments
  (infix-splitter (make-regexp " +")))

; Reply handling

; For the nature of the replies, see RFC 959.

(define (write-replies)
  (let ((replies (the-session-replies)))
    (cond
     ((null? replies)
      (error "no reply registered"))
     (else
      (let loop ((replies replies))
	(if (not (null? replies))
	    (let ((reply-text 
		   (format #f
			   (if (pair? (cdr replies))
			       "~D-~A"
			       "~D ~A")
			   (the-session-reply-code)
			   (car replies))))
	      (display reply-text (the-session-control-output-port))
	      (write-crlf (the-session-control-output-port))
	      (log (syslog-level debug) "Reply: ~A" reply-text)
	      (loop (cdr replies))))))))
  (set-the-session-replies! '())
  (set-the-session-reply-code! #f))

(define (signal-error! code message)
  (replace-reply! code message)
  (signal 'ftpd-error code message))

(define (register-reply! code . messages)
  (if (the-session-reply-code)
      (apply error "tried to register more than one reply" code messages (the-session-replies))
      (apply replace-reply! code messages)))

(define (replace-reply! code . messages)
  (set-the-session-replies! messages)
  (set-the-session-reply-code! code))

