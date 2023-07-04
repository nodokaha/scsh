;;; SMTP client code		-*- Scheme -*- 

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1995 by Brian D. Carlstrom and Olin Shivers.
;;; Copyright (c) 2002-2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; See rfc821: http://www.ietf.org/rfc/rfc0821.txt

;;; SMTP protocol procedures tend to return two values:
;;; - CODE The integer SMTP reply code returned by server for the transaction.
;;; - TEXT A list of strings -- the text messages tagged by the code.
;;; The text strings have the initial code numerals and the terminating
;;; cr/lf's stripped. Codes in the range [1,399] are sucess codes; codes
;;; in the range [400,599] are error codes; codes >= 600 are not part
;;; of the official SMTP spec. This module uses codes >= 600 to indicate
;;; extra-protocol errors. There are two of these:
;;; - 600 Server reply could not be parsed.
;;;   The server sent back some sort of incomprehensible garbage reply.
;;; - 621 Premature EOF while reading server reply.
;;;   The server shut down in the middle of a reply.
;;; A list of the official protocol return codes is appended at the end of
;;; this file.

;;; These little cover functions are trivial packagings of the protocol.
;;; You could write your own to handle, e.g., mailing a message to a list
;;; of addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition-type 'smtp-error '(error))
(define smtp-error? (condition-predicate 'smtp-error))

(define-condition-type 'smtp-recipients-rejected-error '(smtp-error))
(define smtp-recipients-rejected-error?
  (condition-predicate 'smtp-recipients-rejected-error?))

(define (smtp-send-mail from to-list headers body . maybe-host)
  (let* ((host (:optional maybe-host "localhost"))
	 (local (if (string=? host "localhost")
		    (system-name)	; we don't need any DNS for that
		    (system-fqdn)))
	 (connection (smtp-connect host)))
    (receive (code text)
	(smtp-transactions/no-close connection ; Do prologue.
				    (smtp-helo local)
				    (smtp-mail from))
      (if (>= code 400)
	  (begin
	    (smtp-quit (smtp-connection-socket connection))
	    (signal 'smtp-error code text))
	  ;; Send over recipients and collect the losers.
	  (let ((losers (filter-map
			 (lambda (to)
			   (receive (code text)
			       ((smtp-rcpt to) (smtp-connection-socket connection))
			     (and (>= code 400) ; Error
				  (cond ((>= code 600)
					 (smtp-quit
					  (smtp-connection-socket connection))
					 (signal 'smtp-error code text))
					(else `(,to ,code ,@text))))))
			 to-list)))

	    ;; Send the message body and wrap things up.
	    (receive (code text)
		(smtp-transactions connection
				   (smtp-data (normalize-headers headers) body))
	      (if (or (>= code 400)
		      (not (null? losers)))
		  (signal 'smtp-recipients-rejected-error 700 losers))))))))

(define (normalize-headers headers)
  (if (assq 'date headers)
      headers
      (cons (cons 'date
		  (rfc822-time->string (time)))
	    headers)))

(define (smtp-query socket query arg)
  (receive (code text)
      (smtp-transactions socket
			 (smtp-helo (system-name))
			 (query arg))
    (values code text)))

(define (smtp-expand name host)
  (smtp-query (smtp-connect host) smtp-expn name))

(define (smtp-verify name host)
  (smtp-query (smtp-connect host) smtp-vrfy name))

(define (smtp-get-help host . details)
  (smtp-query (smtp-connect host) smtp-help (apply string-append (cons " " details))))

(define (smtp-transactions connection . transactions)
  (let ((socket (smtp-connection-socket connection)))
    (receive (code text) (apply smtp-transactions/no-close connection transactions)
      (cond
       ((or (= code 221)
	    (= code 421))
	(values))
       (else
	(smtp-quit socket)))
      (values code text))))

(define (smtp-transactions/no-close connection . transactions)
  (let loop ((transactions transactions))
    (receive (code text) ((car transactions) (smtp-connection-socket connection))
      (if (or (null? (cdr transactions))
	      (= code 221)
	      (= code 421)		; Redundant, I know.
	      (<= 400 code))
	  (values code text)
	  (loop (cdr transactions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The basics of the protocol

(define (nullary-smtp-command command)
  (lambda (socket)
    (let ((port (socket:outport socket)))
      (write-string command port)
      (write-crlf port))
    (handle-smtp-reply socket)))

(define (unary-smtp-command command)
  (lambda (data)
    (lambda (socket)
      (let ((port (socket:outport socket)))
	(write-string command port)
	(display      #\space port)
	(write-string data    port)
	(write-crlf           port))
      (handle-smtp-reply socket))))

(define-record-type smtp-connection :smtp-connection
  (make-smtp-connection socket)
  smtp-connection?
  (socket smtp-connection-socket))

(define (smtp-connect host . maybe-port)
  (let ((sock (socket-connect protocol-family/internet socket-type/stream host
			      (:optional maybe-port "smtp"))))
    (receive (code text) (handle-smtp-reply sock)
      (if (< code 400)
	  (make-smtp-connection sock)
	  (error "SMTP socket-open server-reply error" sock code text)))))

;; HELLO <local-hostname>
(define smtp-helo (unary-smtp-command "HELO"))

;; MAIL FROM: <sender-address>
(define smtp-mail (unary-smtp-command "MAIL FROM:"))

;; RECIPIENT TO: <destination-address>
(define smtp-rcpt (unary-smtp-command "RCPT TO:"))

;; DATA
(define smtp-data
  (let ((send-DATA-msg (nullary-smtp-command "DATA")))
    (lambda (headers message)		; MESSAGE is a list of strings or an input port.
      (lambda (socket)
	(receive (code text) (send-DATA-msg socket)
	  (if (>= code 400)
	      (values code text)	; Error.

	      ;; We got a positive acknowledgement for the DATA msg,
	      ;; now send the message body.
	      (let ((p (socket:outport socket)))
		(for-each (lambda (pair)
			    (display (symbol->field-name (car pair)) p)
			    (write-char #\: p)
			    (display (cdr pair) p)
			    (write-crlf p))
			  headers)
		(write-crlf p)
		
		(cond ((or (null? message) (pair? message))
		       (for-each (lambda (line)
				   (write-data-line line p))
				 message))
		    
		      ((input-port? message)
		       (let lp ()
			 (let ((stuff (read-line message)))
			   (if (not (eof-object? stuff))
			       (begin
				 (write-data-line stuff p)
				 (newline))))))
		    
		      (else (error "Message must be string or input-port.")))

		(write-crlf p)
		(write-char #\. p)
		(write-crlf p)
		(force-output p)
		(handle-smtp-reply socket))))))))

(define component-charset (char-set-complement (char-set #\-)))

(define (symbol->field-name symbol)
  (let ((components (string-tokenize (symbol->string symbol) component-charset)))
    (string-join (map upcase-string components) "-")))

(define (upcase-string strng)
  (if (string=? "" strng)
      ""
      (string-append (string (char-upcase (string-ref strng 0)))
		     (substring strng 1 (string-length strng)))))

(define (write-data-line line port)
  (display (if (string=? line ".")
	       ".."
	       line)
	   port)
  (write-crlf port))

;; SEND FROM: <sender-address>
(define smtp-send (unary-smtp-command "SEND FROM:"))

;; SEND OR MAIL <sender-address>
(define smtp-soml (unary-smtp-command "SOML FROM:"))

;; SEND AND MAIL <sender-address>
(define smtp-saml (unary-smtp-command "SOML SAML:"))

;; RESET
(define smtp-rset (nullary-smtp-command "RSET"))

;; VERIFY <user>
(define smtp-vrfy (unary-smtp-command "VRFY"))

;; EXPAND <user>
(define smtp-expn (unary-smtp-command "EXPN"))

;; HELP <details>
(define smtp-help
  (let ((send-help (unary-smtp-command "HELP")))
    (lambda details
      (send-help (apply string-append details)))))

;; NOOP
(define smtp-noop (nullary-smtp-command "NOOP"))

;; QUIT
(define smtp-quit
  (let ((quit (nullary-smtp-command "QUIT")))
    (lambda (socket)
      (receive (code text) (quit socket) ; Quit & close socket gracefully.
	(case code		
	  ((221 421))
	  (else (close-socket socket))) ; But close in any event.
	(values code text)))))

;; TURN
(define smtp-turn (nullary-smtp-command "TURN"))

;;; Read and handle the reply. Return an integer (the reply code),
;;; and a list of the text lines that came tagged by the reply code.
;;; The text lines have the reply-code prefix (first 4 chars) and the
;;; terminating cr/lf's stripped.
;;;
;;; In bdc's analog of this proc, he would read another reply if the code was 
;;; in the one-hundred range (1xx). These codes aren't even used in smtp,
;;; according to the RFC. So why?

(define (handle-smtp-reply socket)
  (receive (code text) (read-smtp-reply (socket:inport socket))
    (case code
      ((221 421) (close-socket socket))) ; All done.
    (values code text)))

;;; Read a reply from the SMTP server. Returns two values:
;;; - CODE	Integer. The reply code.
;;; - TEXT	String list. A list of the text lines comprising the reply.
;;;             Each line of text is stripped of the initial reply-code
;;;             numerals (e.g., the first four chars of the reply), and
;;;		the trailing cr/lf. We are in fact generous about what
;;;             we take to be a line -- the protocol requires cr/lf
;;;             terminators, but we'll accept just lf. This appears to
;;;             true to the spirit of the "be strict in what you send,
;;;             and generous in what you accept" Internet protocol philosphy.

(define (read-smtp-reply port)
  (let lp ((replies '()))
    (let ((ln (read-crlf-line port)))
      (if (eof-object? ln)
	  (values 621 (cons "Premature EOF during smtp reply."
			    (reverse replies)))
	  (receive (code line more?) (parse-smtp-reply ln)
	    (let ((replies (cons line replies)))
	      (if more?
		  (lp replies)
		  (values code (reverse replies)))))))))

;;; Parse a line of SMTP reply. Return three values:
;;;   CODE	integer - the reply code that prefixes the string.
;;;   REST	string  - the rest of the line.
;;;   MORE?     boolean - is there more reply to read (i.e., was the numeric 
;;;                       reply code terminated by a "-" character?)

(define (parse-smtp-reply line)
  (if (and (string? line)			; This is all checking
	   (> (string-length line) 3)		; to see if the line
	   (char-numeric? (string-ref line 0))	; is properly formatted.
	   (char-numeric? (string-ref line 1))
	   (char-numeric? (string-ref line 2))
	   (let ((c (string-ref line 3)))
	     (or (char=? c #\space) (char=? c #\-))))

      (values (string->number (substring line 0 3))	; It is.
	      (substring line 4 (string-length line))
	      (char=? (string-ref line 3) #\-))

      (values 600					; It isn't.
	      (string-append "Improperly-formatted smtp reply: " line)
	      #f)))

;;; Reply codes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This material taken from the RFC.
;;;
;;;   1yz   Positive Preliminary reply
;;;
;;;      The command has been accepted, but the requested action
;;;      is being held in abeyance, pending confirmation of the
;;;      information in this reply.  The sender-SMTP should send
;;;      another command specifying whether to continue or abort
;;;      the action.
;;;
;;;         [Note: SMTP does not have any commands that allow this
;;;         type of reply, and so does not have the continue or
;;;         abort commands.]
;;;
;;;   2yz   Positive Completion reply
;;;
;;;      The requested action has been successfully completed.  A
;;;      new request may be initiated.
;;;
;;;   3yz   Positive Intermediate reply
;;;
;;;      The command has been accepted, but the requested action
;;;      is being held in abeyance, pending receipt of further
;;;      information.  The sender-SMTP should send another command
;;;      specifying this information.  This reply is used in
;;;      command sequence groups.
;;;
;;;   4yz   Transient Negative Completion reply
;;;
;;;      The command was not accepted and the requested action did
;;;      not occur.  However, the error condition is temporary and
;;;      the action may be requested again.  The sender should
;;;      return to the beginning of the command sequence (if any).
;;;      It is difficult to assign a meaning to "transient" when
;;;      two different sites (receiver- and sender- SMTPs) must
;;;      agree on the interpretation.  Each reply in this category
;;;      might have a different time value, but the sender-SMTP is
;;;      encouraged to try again.  A rule of thumb to determine if
;;;      a reply fits into the 4yz or the 5yz category (see below)
;;;      is that replies are 4yz if they can be repeated without
;;;      any change in command form or in properties of the sender
;;;      or receiver.  (E.g., the command is repeated identically
;;;      and the receiver does not put up a new implementation.)
;;;
;;;   5yz   Permanent Negative Completion reply
;;;
;;;      The command was not accepted and the requested action did
;;;      not occur.  The sender-SMTP is discouraged from repeating
;;;      the exact request (in the same sequence).  Even some
;;;      "permanent" error conditions can be corrected, so the
;;;      human user may want to direct the sender-SMTP to
;;;      reinitiate the command sequence by direct action at some
;;;      point in the future (e.g., after the spelling has been
;;;      changed, or the user has altered the account status).
;;;
;;;The second digit encodes responses in specific categories:
;;;
;;;   x0z   Syntax -- These replies refer to syntax errors,
;;;         syntactically correct commands that don't fit any
;;;         functional category, and unimplemented or superfluous
;;;         commands.
;;;
;;;   x1z   Information --  These are replies to requests for
;;;         information, such as status or help.
;;;
;;;   x2z   Connections -- These are replies referring to the
;;;         transmission channel.
;;;
;;;   x3z   Unspecified as yet.
;;;
;;;   x4z   Unspecified as yet.
;;;
;;;   x5z   Mail system -- These replies indicate the status of
;;;         the receiver mail system vis-a-vis the requested
;;;         transfer or other mail system action.

;;; Complete list (grouped by function)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 500 Syntax error, command unrecognized
;;;    [This may include errors such as command line too long]
;;; 501 Syntax error in parameters or arguments
;;; 502 Command not implemented
;;; 503 Bad sequence of commands
;;; 504 Command parameter not implemented
;;;  
;;; 211 System status, or system help reply
;;; 214 Help message
;;;    [Information on how to use the receiver or the meaning of a
;;;    particular non-standard command; this reply is useful only
;;;    to the human user]
;;;  
;;; 220 <domain> Service ready
;;; 221 <domain> Service closing transmission channel
;;; 421 <domain> Service not available,
;;;     closing transmission channel
;;;    [This may be a reply to any command if the service knows it
;;;    must shut down]
;;;  
;;; 250 Requested mail action okay, completed
;;; 251 User not local; will forward to <forward-path>
;;; 450 Requested mail action not taken: mailbox unavailable
;;;    [E.g., mailbox busy]
;;; 550 Requested action not taken: mailbox unavailable
;;;    [E.g., mailbox not found, no access]
;;; 451 Requested action aborted: error in processing
;;; 551 User not local; please try <forward-path>
;;; 452 Requested action not taken: insufficient system storage
;;; 552 Requested mail action aborted: exceeded storage allocation
;;; 553 Requested action not taken: mailbox name not allowed
;;;    [E.g., mailbox syntax incorrect]
;;; 354 Start mail input; end with <CRLF>.<CRLF>
;;; 554 Transaction failed
;;;

;;; State diagram
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONNECTION ESTABLISHMENT
;;;     S: 220
;;;     F: 421
;;;  HELO
;;;     S: 250
;;;     E: 500, 501, 504, 421
;;;  MAIL
;;;     S: 250
;;;     F: 552, 451, 452
;;;     E: 500, 501, 421
;;;  RCPT
;;;     S: 250, 251
;;;     F: 550, 551, 552, 553, 450, 451, 452
;;;     E: 500, 501, 503, 421
;;;  DATA
;;;     I: 354 -> data -> S: 250
;;;                       F: 552, 554, 451, 452
;;;     F: 451, 554
;;;     E: 500, 501, 503, 421
;;;  RSET
;;;     S: 250
;;;     E: 500, 501, 504, 421
;;;  SEND
;;;     S: 250
;;;     F: 552, 451, 452
;;;     E: 500, 501, 502, 421
;;;  SOML
;;;     S: 250
;;;     F: 552, 451, 452
;;;     E: 500, 501, 502, 421
;;;  SAML
;;;     S: 250
;;;     F: 552, 451, 452
;;;     E: 500, 501, 502, 421
;;;  VRFY
;;;     S: 250, 251
;;;     F: 550, 551, 553
;;;     E: 500, 501, 502, 504, 421
;;;  EXPN
;;;     S: 250
;;;     F: 550
;;;     E: 500, 501, 502, 504, 421
;;;  HELP
;;;     S: 211, 214
;;;     E: 500, 501, 502, 504, 421
;;;  NOOP
;;;     S: 250
;;;     E: 500, 421
;;;  QUIT
;;;     S: 221
;;;     E: 500
;;;  TURN
;;;     S: 250
;;;     F: 502
;;;     E: 500, 503
