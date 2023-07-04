;;; POP3.scm --- implement the POP3 maildrop protocol in the Scheme Shell
;;
;; $Id: pop3.scm,v 1.1 1998/04/29 07:05:56 ecm Exp $
;;
;;     Copyright (C) 1998  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>


;;; Overview ==============================================================
;;
;; The POP3 protocol allows access to email on a maildrop server. It
;; is often used in configurations where users connect from a client
;; machine which doesn't have a permanent network connection or isn't
;; always turned on, situations which make local SMTP delivery
;; impossible. It is the most common form of email access provided by
;; Internet Service Providers.
;;
;; Two types of authentication are commonly used. The first, most
;; basic type involves sending a user's password in clear over the
;; network, and should be avoided. Unfortunately many POP3 clients
;; only implement this basic authentication. The digest authentication
;; system involves the server sending the client a "challenge" token;
;; the client encodes this token with the pass phrase and sends the
;; coded information to the server. This method avoids sending
;; sensitive information over the network.
;;
;; Once connected, a client may request information about the number
;; and size of the messages waiting on the server, download selected
;; messages (either their headers or the entire content), and delete
;; selected messages.


;;; Entry points =======================================================
;;
;; (pop3:connect [host logfile]) -> connection
;;    Connect to the maildrop server named HOST. Optionally log the
;;    conversation with the server to LOGFILE, which will be appended
;;    to if it exists, and created otherwise. The environment variable
;;    MAILHOST, if set, will override the value of HOST.
;;
;; (pop3:login connection [login password]) -> status
;;    Log in to the mailhost. If a login and password are not
;;    provided, they are first searched for in the user's ~/.netrc
;;    file. USER/PASS authentication will be tried first, and if this
;;    fails, APOP authentication will be tried.
;;
;; (pop3:login/APOP connection login password) -> status
;;    Log in to the mailhost using APOP authentication.
;;
;; (pop3:stat connection) -> integer x integer
;;    Return the number of messages and the number of bytes waiting in
;;    the maildrop.
;;
;; (pop3:get connection msgid) -> status
;;    Download message number MSGID from the mailhost. MSGID must be
;;    positive and less than the number of messages returned by the
;;    pop3:stat call. The message contents are sent to
;;    (current-output-port).
;;
;; (pop3:headers connection msgid) -> status
;;    Download the headers of message number MSGID. The data is sent
;;    to (current-output-port).
;;
;; (pop3:last connection) -> integer
;;    Return the highest accessed message-id number for the current
;;    session. This isn't in the RFC, but seems to be supported by
;;    several servers.
;;
;; (pop3:delete connection msgid) -> status
;;    Mark message number MSGID for deletion. The message will not be
;;    deleted until the client logs out.
;;
;; (pop3:reset connection) -> status
;;    Any messages which have been marked for deletion are unmarked.
;;
;; (pop3:quit connection) -> status
;;    Close the connection with the mailhost.



;;; Portability ======================================================
;;
;; define-record
;; socket, regexp
;; signals/handlers


;;; Related work =====================================================
;;
;; * Emacs is distributed with a C program called movemail which can
;;   be compiled with support for the POP protocol. There is also an
;;   Emacs Lisp library called pop3.el by Richard Pieri which includes
;;   APOP support.
;;
;; * Shriram Krishnamurth has written a POP3 library for MzScheme (as
;;   well as support for the NNTP protocol, for SMTP, ...).
;;
;; * Siod (a small-footprint Scheme implementation by George Carette)
;;   includes support for the POP3 protocol.
;;
;; * rfc1939 describes the POP3 protocol.




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

;;: [host x logfile] -> connection
(define (pop3:connect . args)
  (let* ((host (or (getenv "MAILHOST")
                   (safe-first args)))
         (logfile (safe-second args))
         (LOG (and logfile
                   (open-output-file logfile
                                     (if (file-exists? logfile)
                                         (bitwise-ior open/write open/append)
                                         (bitwise-ior open/write open/create))
                                     #o600)))
         (hst-info (host-info host))
         (hostname (host-info:name hst-info))
         (srvc-info (service-info "pop-3" "tcp"))
         (sock (socket-connect protocol-family/internet
                               socket-type/stream
                               hostname
                               (service-info:port srvc-info)))
         (connection (make-pop3-connection hostname
                                           sock
                                           LOG "" "" #f #f)))
    (pop3:log connection
              (format #f "~%-- ~a: opened POP3 connection to ~a"
                      ;; (date->string (date))
                      "Dummy date"      ; (format-time-zone) is broken in v0.5.1
                      hostname))

    ;; read the challenge the server sends in its welcome banner
    (let* ((banner (pop3:read-response connection))
           (match (string-match "\\+OK .* (<[^>]+>)" banner))
           (challenge (and match (match:substring match 1))))
      (set-pop3-connection:challenge connection challenge))
           
    connection))


;; first try standard USER/PASS authentication, and switch to APOP
;; authentication if the server prefers.
;;: [string x string] -> status
(define (pop3:login connection . args)
  (let ((login (or (safe-first args)
                   (netrc:login (pop3-connection:host-name connection))
                   (call-error "must provide a login" pop3:login args)))
        (password (or (safe-second args)
                      (netrc:password (pop3-connection:host-name connection))
                      (call-error "must provide a password" pop3:login args))))
    (with-handler
     (lambda (result punt)
       (if (-ERR? result)
           (if (pop3-connection:challenge connection)
               (pop3:login/APOP connection login password)
               (error "login failed"))))
     (lambda ()
       (pop3:send-command connection (format #f "USER ~a" login))
       (pop3:send-command connection (format #f "PASS ~a" password))
       (set-pop3-connection:login connection login)
       (set-pop3-connection:password connection password)
       (set-pop3-connection:state connection 'connected)))))


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
;;: connection x string x string -> status
(define (pop3:login/APOP connection login password)
  (let* ((key (string-append (pop3-connection:challenge connection)
                             password))
         (digest (md5-digest key))
         (status (pop3:send-command connection
                                    (format #f "APOP ~a ~a" login digest))))
  (set-pop3-connection:login connection login)
  (set-pop3-connection:password connection password)
  (set-pop3-connection:state connection 'connected)
  status))
  

;; return number of messages and number of bytes waiting at the maildrop
;;: connection -> integer x integer
(define (pop3:stat connection)
  (pop3:check-transaction-state connection 'pop3:stat)
  (let* ((response (pop3:send-command connection "STAT"))
         (match (string-match "([0-9]+) ([0-9]+)" response)))
    (values (string->number (match:substring match 1))
            (string->number (match:substring match 2)))))

;; dump the message number MSGID to (current-output-port)
;;: connection x integer -> status
(define (pop3:get connection msgid)
  (pop3:check-transaction-state connection 'pop3:get)
  (let ((status (pop3:send-command connection (format #f "RETR ~a" msgid))))
    (pop3:dump (socket:inport (pop3-connection:command-socket connection)))
    status))

;;: connection x integer -> status
(define (pop3:headers connection msgid)
  (pop3:check-transaction-state connection 'pop3:headers)
  (let ((status (pop3:send-command connection (format #f "TOP ~a 0" msgid))))
    (pop3:dump (socket:inport (pop3-connection:command-socket connection)))
    status))

;; Return highest accessed message-id number for the session. This
;; ain't in the RFC, but seems to be supported by several servers.
;;: connection -> integer
(define (pop3:last connection)
  (pop3:check-transaction-state connection 'pop3:last)
  (let ((response (pop3:send-command connection "LAST")))
    (string->number (car ((infix-splitter) response)))))

;; mark the message number MSGID for deletion. Note that the messages
;; are not truly deleted until the QUIT command is sent, and messages
;; can be undeleted using the RSET command.
;;: connection x integer -> status
(define (pop3:delete connection msgid)
  (pop3:check-transaction-state connection 'pop3:delete)
  (pop3:send-command connection (format #f "DELE ~a" msgid)))


;; any messages which have been marked for deletion are unmarked
;;: connection -> status
(define (pop3:reset connection)
  (pop3:check-transaction-state connection 'pop3:reset)
  (pop3:send-command connection "RSET"))

;;: connection -> status
(define (pop3:quit connection)
  (pop3:check-transaction-state connection 'pop3:quit)
  (let ((status (pop3:send-command connection "QUIT")))
    (close-socket (pop3-connection:command-socket connection))
    status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nothing exported below.

(define-record pop3-connection
  host-name
  command-socket
  logfd
  login
  password
  challenge
  state)

;; cf /usr/local/src/scheme48-0.49/scheme/rts/condition.scm
(define-condition-type '-ERR '(error))
(define -ERR? (condition-predicate '-ERR))


(define (pop3:check-transaction-state connection caller)
  (if (not (eq? (pop3-connection:state connection) 'connected))
      (call-error "not in transaction state" caller)))

(define (pop3:read-response connection)
  (let* ((sock (pop3-connection:command-socket connection))
         (IN (socket:inport sock))
         (line (read-line IN)))
    (pop3:log connection (format #f "-> ~a" line))
    line))

;; this could perhaps be improved
(define (pop3:handle-response response command)
  (let ((match (string-match "^\\+OK (.*)" response)))
    (if match (match:substring match 1)
        (let ((match2 (string-match "^-ERR (.*)" response)))
          (signal '-ERR (match:substring match2 1) command)))))


(define (pop3:log connection line)
  (let ((LOG (pop3-connection:logfd connection)))
    (and LOG
         (write-string line LOG)
         (write-string "\n" LOG)
         (force-output LOG))))

(define (pop3:send-command connection command)
  (let* ((sock (pop3-connection:command-socket connection))
         (OUT (socket:outport sock)))
    (write-string command OUT)
    (write-crlf OUT)
    (pop3:log connection (format #f "<- ~a" command))
    (pop3:handle-response (pop3:read-response connection) command)))


;; who will write this in Scheme?
(define (md5-digest str)
  (car (run/strings (md5 ,str))))

(define (pop3:dump fd)
  (let loop ((line (read-line fd)))
       (cond ((and (not (eof-object? line))
                   (not (equal? line ".\r")))
              (and (eq? 0 (index line #\.)) ; fix byte-stuffed lines
                   (eq? 1 (index line #\. 1))
                   (set! line (substring line 1 (string-length line))))
	      (write-string line)
              (newline)
	      (loop (read-line fd))))))

;; EOF
