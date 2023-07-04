;;; pop3.scm --- implement the POP3 maildrop protocol in the Scheme Shell

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998 by Eric Marsden
;;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; Related work =====================================================
;;
;; * Emacs is distributed with a C program called movemail which can
;;   be compiled with support for the POP protocol. There is also an
;;   Emacs Lisp library called pop3.el by Richard Pieri which includes
;;   APOP support.
;;
;; * Shriram Krishnamurthi has written a POP3 library for MzScheme (as
;;   well as support for the NNTP protocol, for SMTP, ...).
;;
;; * Siod (a small-footprint Scheme implementation by George Carette)
;;   includes support for the POP3 protocol.
;;
;; * rfc1939 describes the POP3 protocol.
;;   http://www.ietf.org/rfc/rfc1939.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication is initiated by the client. The server responds to
;; each request with a status indicator and an explanatory message.
;; The client starts off by opening a connection to a well known port
;; on the server machine (typically TCP 110, or 109 on some broken
;; systems). Messages sent to the server are of the form
;; 
;;            CMD [ <space> arg ] <CR> <LF>
;;
;; Replies from the server are of the form
;;
;;            status [ <space> Informative message ] <CR> <LF>
;;
;; where status is either "+OK" or "-ERR". If the server is sending
;; data (the contents of a message for example), it marks the end of
;; the data by a line consisting only of a decimal point (thus the
;; bytes to look out for are <CR><LF>.<CR><LF>. Any lines in the data
;; starting with a . have an additional . added to the beginning, to
;; avoid the client thinking that the line marks the end of the
;; message. The client should therefore replace double decimal points
;; at the beginning of a line by a single decimal point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pop3-connect . args)
  (let-optionals args ((host-arg #f)
		       (login #f)
		       (password #f)
		       (log #f))
    (let* ((host (or host-arg
		     (getenv "MAILHOST")))
	   (hst-info (host-info host))
	   (hostname (host-info:name hst-info))
	   (srvc-info (service-info "pop3" "tcp"))
	   (sock (socket-connect protocol-family/internet
				 socket-type/stream
				 hostname
				 (service-info:port srvc-info)))
	   (connection (make-pop3-connection hostname
					     sock
					     log "" "" #f #f)))
      (pop3-log connection
		(string-append "-- "
			       (date->string (date))
			       ": opened POP3 connection to "
			       hostname))

      ;; read the challenge the server sends in its welcome banner
      (let* ((banner (read-response connection))
	     (match (regexp-search (rx (: "+OK " (* (~ #\<))
					  #\< (submatch (+ (~ #\>))) #\>))
				   banner))
	     (challenge (and match (match:substring match 1))))
	(set-pop3-connection-challenge! connection challenge))
      
      (pop3-login connection login password)
      
      connection)))

;; first try standard USER/PASS authentication, and switch to APOP
;; authentication if the server prefers.

(define (pop3-login connection login password)
  (let* ((netrc-record #f)
	 (get-netrc-record
	  (lambda ()
	    (cond
	     (netrc-record)
	     (else
	      (set! netrc-record
		    (netrc-machine-entry (pop3-connection-host-name connection) #f))
	      netrc-record)))))
    (let ((login (or login
		     (begin
 		       (if (or (not (get-netrc-record))
			       (not (netrc-entry-login (get-netrc-record))))
			   (signal 'pop3-error
				   "no login record specified and no netrc entry"))
		       (netrc-entry-login (get-netrc-record)))))
	  (password (or password
			(begin 
			  (if (not (netrc-entry-password (get-netrc-record)))
			      (signal 'pop3-error
				      "no password record specified and no netrc entry"))
			  (netrc-entry-password (get-netrc-record))))))
      (with-fatal-error-handler*
       (lambda (result punt)
	 (cond
	  ((not (pop3-error? result)) (punt))
	  ((pop3-connection-challenge connection)
	   (pop3-apop-login connection login password))))
       (lambda ()
	 (send-command connection (build-command "USER" login))
	 (send-command connection (build-command "PASS" password))
	 (set-pop3-connection-login! connection login)
	 (set-pop3-connection-password! connection password)
	 (set-pop3-connection-state! connection 'connected))))))

;; Login to the server using APOP authentication (no cleartext
;; passwords are sent over the network). The server appends a token to
;; its welcome message, which is built from the server's fully
;; qualified domain name and a unique serial number. The client
;; concatenates this token and the pass phrase and applies the MD5
;; digest algorithm (a one-way hash) to produce a digest. The user
;; name and the digest are sent to the server to authenticate the
;; user. The following example comes from the RFC:
;;
;;      S: +OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>
;;      C: APOP mrose c4c9334bac560ecc979e58001b3e22fb
;;      S: +OK maildrop has 1 message (369 octets)
;; 
;;      In this example, the shared  secret  is  the  string  `tan-
;;      staaf'.  Hence, the MD5 algorithm is applied to the string
;; 
;;         <1896.697170952@dbc.mtview.ca.us>tanstaaf
;; 
;;      which produces a digest value of
;; 
;;         c4c9334bac560ecc979e58001b3e22fb
;;         

(define (pop3-apop-login connection login password)
  (let* ((key (string-append (pop3-connection-challenge connection)
                             password))
         (digest (number->string
		  (md5-digest->number (md5-digest-for-string key))
		  16))
         (status (send-command connection
			       (build-command "APOP" login digest))))
    (set-pop3-connection-login! connection login)
    (set-pop3-connection-password! connection password)
    (set-pop3-connection-state! connection 'connected)
    status))

;; return number of messages and number of bytes waiting at the maildrop

(define (pop3-stat connection)
  (check-transaction-state connection pop3-stat)
  (let* ((response (send-command connection "STAT"))
         (match (regexp-search (rx (posix-string "([0-9]+) ([0-9]+)")) response)))
    (values (string->number (match:substring match 1))
            (string->number (match:substring match 2)))))

(define (pop3-retrieve-message connection msgid)
  (check-transaction-state connection pop3-retrieve-message)
  (let* ((status (send-command connection
			       (build-command "RETR" (number->string msgid))))
	 (port (socket:inport (pop3-connection-command-socket connection)))
	 (headers (read-rfc822-headers port read-crlf-line))
	 (body (multiline-response->lines port)))
    (values headers body)))

(define (pop3-retrieve-headers connection msgid)
  (check-transaction-state connection pop3-retrieve-headers)
  (let* ((status (send-command connection
			       (build-command "TOP" (number->string msgid) "0")))
	 (port (socket:inport (pop3-connection-command-socket connection)))
	 (headers (read-rfc822-headers port read-crlf-line)))
    (exhaust-multiline-response port)
    headers))

;; Return highest accessed message-id number for the session. This
;; ain't in the RFC, but seems to be supported by several servers.

(define (pop3-last connection)
  (check-transaction-state connection pop3-last)
  (let ((response (send-command connection "LAST")))
    (string->number (car ((infix-splitter) response)))))

;; mark the message number MSGID for deletion. Note that the messages
;; are not truly deleted until the QUIT command is sent, and messages
;; can be undeleted using the RSET command.

(define (pop3-delete connection msgid)
  (check-transaction-state connection pop3-delete)
  (send-command connection (build-command "DELE" (number->string msgid)))
  (values))


;; any messages which have been marked for deletion are unmarked

(define (pop3-reset connection)
  (check-transaction-state connection pop3-reset)
  (send-command connection "RSET")
  (values))

(define (pop3-quit connection)
  (check-transaction-state connection pop3-quit)
  (let ((status (send-command connection "QUIT")))
    (close-socket (pop3-connection-command-socket connection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nothing exported below.

(define-record-type pop3-connection :pop3-connection
  (make-pop3-connection host-name command-socket log-port login password challenge state)
  pop3-connection?
  (host-name pop3-connection-host-name)
  (command-socket pop3-connection-command-socket)
  (log-port pop3-connection-log-port)
  (login pop3-connection-login set-pop3-connection-login!)
  (password pop3-connection-password set-pop3-connection-password!)
  (challenge pop3-connection-challenge set-pop3-connection-challenge!)
  (state pop3-connection-state set-pop3-connection-state!))

(define-condition-type 'pop3-error '(error))
(define pop3-error? (condition-predicate 'pop3-error))

(define (check-transaction-state connection caller)
  (if (not (eq? (pop3-connection-state connection) 'connected))
      (error "not in transaction state" caller)))

(define (read-response connection)
  (let* ((sock (pop3-connection-command-socket connection))
         (in (socket:inport sock))
         (line (read-crlf-line in)))
    (pop3-log connection (string-append "-> " line))
    line))

;; this could perhaps be improved
(define (handle-response response command)
  (let ((match (regexp-search (rx (posix-string "^\\+OK(.*)")) response)))
    (if match 
	(match:substring match 1)
        (let ((match2 (regexp-search (rx (posix-string "^-ERR(.*)")) response)))
	  (if match2
	      (signal 'pop3-error (match:substring match2 1) command)
	      (signal 'pop3-error response command))))))


(define (pop3-log connection line)
  (let ((log (pop3-connection-log-port connection)))
    (if log
	(begin
	  (write-string line log)
	  (newline log)
	  (force-output log)))))

(define (send-command connection command)
  (let* ((sock (pop3-connection-command-socket connection))
         (out (socket:outport sock)))
    (write-string command out)
    (write-crlf out)
    (pop3-log connection (string-append "<- " command))
    (handle-response (read-response connection) command)))

(define (multiline-response->lines port)
  (let loop ((reverse-lines '()))
    (let ((line (read-crlf-line port)))
      (if (and (not (eof-object? line))
	       (not (string=? line ".")))
	  (let ((line (if (string-prefix? ".." line)
			  (substring line 1 (string-length line))
			  line)))
	    (loop (cons line reverse-lines)))
	  (reverse reverse-lines)))))

(define (exhaust-multiline-response port)
  (let loop ()
    (let ((line (read-crlf-line port)))
      (if (and (not (eof-object? line))
	       (not (string=? line ".")))
	  (loop)))))

(define (build-command str . opt-args)
  (string-join (cons str opt-args)))
