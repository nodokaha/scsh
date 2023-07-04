;; The SUrflet handler
;; Copyright Andreas Bernauer, 2002

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS

;;; DEBUGging
(define *debug* #t)

;;; OPTIONS for the SUrflet handler.
;; Preserved thread fluid because between different calls to
;; surflet-handler the options shall remain the same. SURFLET-HANDLER
;; sets the value (an option record, see end of file)
(define *options* (make-preserved-thread-fluid #f))

;;; TABLES are thread safe as they all use the same lock and different
;;; keys for the hash (There may be performance reasons to change
;;; this, though).

;;; SURFLET-TABLE cache
(define *surflet-table* (make-string-table)) ; path-string is index
(define *surflet-table-lock* (make-lock))

;;; SESSION-TABLE
;; Every session gets an entry in the hash table. Entries are session
;; records.
(define *session-table* (make-integer-table)) ; session-id is index
(define *session-table-lock* (make-lock))

;; INSTANCE is the session that is currently handled.
(define *instance* (make-thread-cell #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SURFLET-HANDLER

;;; SURFLET-HANDLER
;; Loads a new or resumes a suspended SUrflet; returns a
;; (HTTP-)RESPONSE. SURFLET-PATH is a string pointing to the real
;; directory where the SUrflets are searched.
(define (surflet-handler options)
  (set-thread-fluid! *options* options)
  (spawn surveillance-thread)
  (lambda (path req)
    (if (pair? path)			; need at least one element
	(let ((request-method (request-method req))
	      (path-string (uri-path->uri path)))
	  (if (or (string=? request-method "GET")
		  (string=? request-method "POST"))
	      (make-input-response
	       (lambda (input-port)
		 (let ((s-req (make-surflet-request req input-port)))
		   (if (resume-url? path-string)
		       (resume-url path-string (options-surflet-path) s-req)
		       (launch-new-session path-string (options-surflet-path) s-req)))))
	      (make-error-response (status-code method-not-allowed) req 
				   request-method)))
	(make-error-response (status-code bad-request) req 
			     (format #f "Bad path: ~s" path)))))

;;; LAUNCH-NEW-SESSION
;; Loads and runs a new session of a SUrflet installing the RESET
;; boundary; returns a (HTTP-)RESPONSE. PATH-STRING is the virtual
;; path of the request, SURFLET-PATH is a string pointing to the real
;; directory of the SUrflets, and S-REQ the request of the browser.
(define (launch-new-session path-string surflet-path s-req)
  (cond 

   ((file-not-exists? (absolute-file-name path-string surflet-path))
    (make-error-response (status-code not-found) 
			 (surflet-request-request s-req) path-string))

   ((string=? (file-name-extension path-string) ".scm")
    (receive (session-id lifetime)
	(with-lock *session-table-lock*
	  (let ((session-id (generate-new-table-id *session-table*))
		(lifetime (options-session-lifetime)))
	    (table-set! *session-table* session-id
			(make-session path-string          ; used for redirections
				      (make-integer-table) ; continuation table
				      (make-lock)          ; continuation table lock
				      (make-thread-safe-counter) ; continuation counter
				      #f	                  ; session-data
				      lifetime))
	    (values session-id lifetime)))

      ;; no access to session table until new session-id is saved
      (register-instance! session-id)
      
      (with-fatal-error-handler
       ;; Catch conditions from get-surflet-rt-structure.
       (lambda (condition decline)
	 (delete-session! session-id)
	 (bad-surflet-error-response s-req path-string condition))
       (let ((surflet (get-surflet-rt-structure path-string surflet-path)))
	 (timeout-queue-register-session! session-id (+ (time) lifetime))

	 (reset
	  (with-fatal-error-handler
	   ;; Catch conditions that occur while running the surflet.
	   (lambda (condition decline)
	     (delete-session! session-id)
	     ;; Restore correct continuation with shift.
	     (shift unused
		    (bad-surflet-error-response s-req path-string condition)))
	   (with-cwd surflet-path
	     (with-names-from-rt-structure 
	      surflet surflet-interface 
	      (main s-req)))))))))	; Launch serlvet's main procedure.

   (else				; We'll serve every non-scm file.
    (make-error-response (status-code forbidden) 
			 (surflet-request-request s-req)
			 "Can't serve other than Scheme files."
			 path-string))
   ))


;;; SESSION-SURVEILLANCE     
(define *timeout-queue*)

(define (timeout-queue-register-session! session-id timeout)
  (search-tree-set! *timeout-queue* (cons session-id timeout) 'ignore))

(define (timeout-queue-remove-session! session-id)
  (search-tree-set! *timeout-queue* (cons session-id 0) #f))

(define (timeout-queue-adjust-session-timeout! session-id new-timeout)
  (search-tree-set! *timeout-queue* (cons session-id new-timeout) 'ignore))


(define (surveillance-thread)
  (set! *timeout-queue* (make-search-tree (lambda (p q) (eq? (car p) (car q)))
                                          (lambda (p q)
                                            (< (cdr p) (cdr q)))))
  (let lp ()
    (with-lock *session-table-lock*
      (let ((now (time)))
	(let lp2 ()
	  (receive (session-id.time ignore) (search-tree-min *timeout-queue*)
	    (if session-id.time
		(if (<= (cdr session-id.time) now)
		    (let ((session-id (car session-id.time)))
		      (table-set! *session-table* session-id #f)
		      (pop-search-tree-min! *timeout-queue*)
		      (lp2))))))))
    (sleep 1000)
    (lp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RESUME-URL
;; Resumes a suspended URL and returns a (HTTP-)RESPONSE. PATH-STRING
;; is the virtual path, SURFLET-PATH a string pointing to the real
;; directory of the SUrflets and S-REQ the request of the browser.
(define resume-url
  (let ((bad-request 
	 (lambda (path-string s-req)
	   (make-error-response
	    (status-code bad-request) 
	    (surflet-request-request s-req)
            ((options-make-session-timeout-text)
             (resume-url-surflet-name path-string)))))
	(lookup-continuation-table
	  (lambda (session continuation-table continuation-id)
	    (let ((continuation-table-lock (session-continuation-table-lock session)))
	      (with-lock continuation-table-lock
		(table-ref continuation-table continuation-id))))))

    (lambda (path-string surflet-path s-req)
      (receive (session-id continuation-id)
	  (resume-url-ids  path-string)
	;; Try to get continuation-table and then the continuation.
	(let ((session (session-lookup session-id)))
	  (if session
	      (let* ((continuation-table (session-continuation-table session))
		     (resume (lookup-continuation-table session continuation-table 
							continuation-id)))
		(if resume
		    (with-cwd surflet-path
		      (begin
			(register-instance! session-id)
			(session-adjust-timeout! session-id)
			(resume s-req)))
		    (bad-request path-string s-req)))
	      (bad-request path-string s-req)))
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SURFLET-INTERFACE

;;; SEND/SUSPEND
;; Suspends current computation, saves current continuation, and
;; leaves current continuation via SHIFT with a (HTTP-)RESPONSE.
;; RESPONSE-MAKER is a procedure returnig a SURFLET-RESPONSE (that is,
;; eventually converted to a HTTP-RESPONSE).
(define (send/suspend response-maker)
  (shift return
	 (let* ((session-id (instance-session-id))
		(session (session-lookup session-id)))
	   ;;  the instance might be deleted in the meanwhile
	   (if session
	       (let ((continuations-table (session-continuation-table session))
		     (continuation-table-lock (session-continuation-table-lock session))
		     (continuation-counter (session-next-continuation-counter session)))
		 (let ((continuation-id 		   
			(with-lock continuation-table-lock
			  (let ((c-id (generate-new-table-id continuations-table)))
			    (table-set! continuations-table c-id return)
			    c-id))))
		   (let ((new-url (make-resume-url (session-surflet-name session)
						   session-id 
						   continuation-counter
						   continuation-id)))
		     (make-http-response (response-maker new-url)))))
	       (make-error-response (status-code not-found) #f
					 "The URL refers to a SUrflet, whose session is no longer alive.")))))

;;; SEND/FINISH
;; Kills current session, and leaves current continuation returning
;; via SHIFT with a (HTTP-)RESPONSE. RESPONSE is a SURFLET-RESPONSE.
(define (send/finish response)
  (delete-session! (instance-session-id))
  (shift unused (make-http-response response)))


;;; SEND
;; Leaves current continuation via SHIFT with a
;; (HTTP-)RESPONSE. RESPONSE is a SURFLET-RESPONSE.
(define (send response)
  (shift unused (make-http-response response)))

;;; SEND-ERROR
;; Stops current computation, and leaves current continuation via
;; SHIFT with a (HTTP-)(ERROR-)RESPONSE. STATUS-CODE is a status code
;; from HTTP-RESPONSES, S-REQ a surflet-request (may be #f) and
;; MESSAGES contains further informations (arbitrary types).
(define (send-error status-code s-req . messages)
  (shift unused (apply make-error-response 
		       (cons status-code 
			     (cons (and (surflet-request? s-req)
					(surflet-request-request s-req))
				   messages)))))

;;; MAKE-HTTP-RESPONSE
;; Converts a SURFLET-RESPONSE to a (HTTP-)RESPONSE. Returns a
;; (HTTP-)RESPONSE.
(define (make-http-response response)
  (cond
   ((surflet-response? response)
    (let ((data (surflet-response-data response)))
      (if (valid-surflet-response-data? data)
	  (make-response
	   (surflet-response-status response)
	   #f
	   (time)
	   (surflet-response-content-type response)
	   (surflet-response-headers response)
	   (make-writer-body 
	    (lambda (out options)
	      (cond
	       ((string? data) (display data out))
	       ((list? data)   (for-each (lambda (data) (display data out)) data))
	       (else  ;; We lose. 
		(display "Error in SUrflet output: no valid data.\n" out))
	       ))))
	  (make-error-response (status-code internal-error) #f 
			       "The SUrflet returned an invalid response object (wrong data type in surflet-response)."))))
   ((and (response? response)		;; RESPONSE? refers to a HTTP-RESPONSE.
	 (redirect-body? (response-body response)))
    response)
   (else
    (make-error-response (status-code internal-error) #f
			 "The SUrflet returned an invalid response object (no surflet-response)."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SESSIONS

;;; SESSION-LOOKUP
;; Looks up SESSION-ID in the *SESSION-TABLE* (locking) and returns
;; the SESSION record, if anby (#f otherwise).
(define (session-lookup session-id)
  (with-lock *session-table-lock*
    (table-ref *session-table* session-id)))

;;; SESSION-NEXT-CONTINUATION-COUNTER
;; Increases the SESSION-CONTINUATION-COUNTER in the SESSION record by
;; one.
(define (session-next-continuation-counter session)
  (thread-safe-counter-next! 
   (session-continuation-counter session)))

;;; DELETE-SESSION!
;; Deletes the session indicated by its number SESSION-ID from the
;; *SESSION-TABLE* (locking).
(define (delete-session! session-id)
  (with-lock *session-table-lock*
    ;; notify surveillance of session being alread killed (prevents
    ;; surveillance of killing new session that has the same number by
    ;; accident)
  (let ((session (table-ref *session-table* session-id)))
    (if session
	(begin
	  (timeout-queue-remove-session! session-id)
	  (table-set! *session-table* session-id #f))
	;; else: somebody was faster than we
	))))


 
;;; SESSION-ADJUST-TIMEOUT!
;; Resets time-to-die of session indicated by its SESSION-ID number.
(define (session-adjust-timeout! session-id . maybe-time-to-live)
  (really-session-adjust-timeout!
   session-id 
   (:optional maybe-time-to-live (session-lifetime session-id))))

(define (really-session-adjust-timeout! session-id time-to-live)
  (with-lock *session-table-lock*
    (let ((session (table-ref *session-table* session-id)))
      (if session
	  (timeout-queue-adjust-session-timeout!
	   session-id
	   (+ (time) time-to-live))
	  (error "There is no session with this ID" session-id)))))

;;; ADJUST-TIMEOUT!
;; Resets time-to-die of current session. The argument must be
;; optional as PLT does not have it.
(define (adjust-timeout! . maybe-time-to-live)
  (let ((session-id (instance-session-id)))
    (really-session-adjust-timeout! 
     session-id
     (:optional maybe-time-to-live
                (session-lifetime session-id)))))

(define (session-lifetime session-id)
  (cond ((session-lookup session-id)
	 => (lambda (session)
	      (really-session-lifetime session)))
	(else #f)))

(define (set-session-lifetime! session-id new-lifetime)
  (cond ((session-lookup session-id)
	 => (lambda (session)
	      (really-set-session-lifetime! session new-lifetime)
	      (session-adjust-timeout! session-id new-lifetime)))
	(else #f)))

;; unused and not exported
;;; RESET-SESSION-TABLE!
;; Clears the *SESSION-TABLE* (locking)
(define (reset-session-table!)
  (with-fatal-error-handler
   (lambda (condtion decline)
     (release-lock *session-table-lock*)
     (decline))
   (with-lock *session-table-lock*
     ;; notify session killing
     (table-walk 
      (lambda (session-id session)
	(timeout-queue-remove-session! session-id))
      *session-table*)
     (set! *session-table* (make-integer-table)))))

;;; GET-SESSIONS
;; Returns a list of all active sessions in *SESSION-TABLE*
;; (locking). The user only gets the session-id, so nothing will
;; happen, if he saves this number. (Otherwise, if he saves the
;; sessions, they will never be GC'ed). From the user's point of view,
;; the number behaves like a record of type session.
(define (get-sessions)
  (with-lock *session-table-lock*
    (let ((sessions '()))
      (table-walk
       (lambda (session-id session-entry)
	 (set! sessions (cons session-id sessions)))
       *session-table*)
      sessions)))

(define (get-session session-id)
  session-id)

;; SESSION-ALIVE? returns #t if there is a session with this id, #f
;; otherwise.
(define (session-alive? session-id)
  (if (session-lookup session-id) #t #f))

;;; GET-CONTINUATIONS
;; Returns a list of all continuations of the session indicated by the
;; SESSION-ID number (locking). The user only gets the pair
;; (session-id . continuation-id), so nothing will happen, if he saves
;; this number. (Otherwise, if he saves the continuations, they will
;; never be GC'ed). From the user's point of view, the number behaves
;; like a record of type continuation.
(define (get-continuations session-id)
  (let ((session (session-lookup session-id)))
    (if session
	(let ((continuation-table-lock (session-continuation-table-lock session))
	      (continuation-table (session-continuation-table session))
	      (continuations '()))
	  (with-lock continuation-table-lock
	    (table-walk
	     (lambda (continuation-id continuation-entry)
	       (set! continuations (cons (cons session-id continuation-id)
					 continuations)))
	     continuation-table)
	    continuations))
	'())))

;;; DELETE-CONTINUATION
;; Deletes continuation SESSION-ID, CONTINUATION-ID (locking).
(define (delete-continuation! session-continuation-id)
  (let* ((session-id (car session-continuation-id))
	 (continuation-id (cdr session-continuation-id))
	 (session (session-lookup session-id)))
    (if session
	(let ((continuation-table-lock (session-continuation-table-lock session)))
	  (with-lock continuation-table-lock
	    (let ((continuation-table (session-continuation-table session)))
	      (if (table-ref continuation-table continuation-id)
		  (table-set! continuation-table continuation-id #f))))))))

(define (continuation-id session-continuation-id) 
  (cdr session-continuation-id))

;;; SET-SESSION-DATA!, GET-SESSION-DATA
;; Access to arbitrary data stored along with current session (no
;; locking!).
(define (set-session-data! new-data)
  (let ((session (session-lookup (instance-session-id))))
    (if session
	(set-session-session-data! session new-data)
	(error "Instance no longer alive."))))

(define (get-session-data)
  (let ((session (session-lookup (instance-session-id))))
    (if session
	(session-session-data session)
	(error "Instance no longer alive."))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID generation  

;;; GENERATE-NEW-TABLE-ID
;; Returns a random integer not used in the hash TABLE (no
;; locking!). The locking has to happe elsewhere. 
(define (generate-new-table-id table)
  (let loop ((id (random)))
    (if (table-ref table id)
	;; FIXME?: this may loop forever, if the table is full (can
	;; this ever happen?)
	(loop (random))
	id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SURFLETs CACHE

;;; GET-SURFLET-RT-STRUCTURE
;; Returns SUrflet's RT-STRUCTURE indicated by SURFLET-NAME (a virtual
;; path string) while managing the SUrflet cache *SURFLET-TABLE*
;; (locking).
(define get-surflet-rt-structure
  (let ((load-surflet 
	 (lambda (full-surflet-name cached?)
	   ;; load-config-file does not care about cwd(?)
	   ;; --> absolute file name needed
	   (load-config-file full-surflet-name)
	   ;; surflet-structure to load must be named "surflet"
	   (let ((surflet-structure (reify-structure 'surflet)))
	     (load-structure surflet-structure)
	     (if cached?
		 (table-set! *surflet-table* full-surflet-name 
			     (cons surflet-structure
				   (file-last-mod full-surflet-name))))
	     surflet-structure))))

    (lambda (surflet-name directory)
      (let ((full-surflet-name (absolute-file-name surflet-name directory)))
	(if (options-cache-surflets?)
	    (with-lock *surflet-table-lock*
	      (cond
	       ((table-ref *surflet-table* full-surflet-name) =>
		(lambda (surflet)
		  (if (equal? (file-last-mod full-surflet-name) 
			      (cdr surflet))
		      (car surflet)
		      (load-surflet full-surflet-name #t))))
	       (else
		(load-surflet full-surflet-name #t))))
	    (load-surflet full-surflet-name #f))))))

;;; GET-LOADED-SURFLETS
;; Returns list of all loaded surflets (real path strings).
(define (get-loaded-surflets)
  (with-lock *surflet-table-lock*
    (let ((loaded-surflets '()))
      (table-walk
       (lambda (surflet-path rt-structure)
	 (set! loaded-surflets (cons surflet-path loaded-surflets)))
       *surflet-table*)
      loaded-surflets)))

;;; UNLOAD-SURFLET
;; Removes SURFLET-NAME from the *SURFLET-TABLE* cache (locking).
(define (unload-surflet surflet-name)
  (with-lock *surflet-table-lock*
    (if (table-ref *surflet-table* surflet-name)
	(table-set! *surflet-table* surflet-name #f))))

;;; RESET-SURFLET-CACHE!
;; Clears *SURFLET-TABLE* (locking).
(define (reset-surflet-cache!)
  (with-lock *surflet-table-lock*
    (set! *surflet-table* (make-string-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSTANCE

;;; REGISTER-INSTANCE!
;; Saves values for current session (in a record).
(define (register-instance! session-id)
  (thread-cell-set! *instance* 
		    (make-instance session-id)))


;;; INSTANCE-SESSION-ID
;; Returns session-id of current *INSTANCE*.
(define (instance-session-id)
  (really-instance-session-id (thread-cell-ref *instance*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RESUME-URL

;; Functions and constants for generating and parsing Continuation
;; URLs (= resume URLs). Resume URLs look like
;; http://localhost:8088/surflet/admin-handler.scm;k757033335;c1-684902143?return54=
(define *resume-url-regexp* (rx (submatch (* (- printing ";"))) 
				";k" (submatch (* digit)) ; Instance-ID
				";c" (+ digit)            ; Continuation Counter
				"-" (submatch (* digit)))) ; Continuation-ID

;; All arguments are numbers except PATH-STRING, which is a string.
(define (make-resume-url path-string session-id continuation-counter continuation-id)
  (string-append (file-name-nondirectory path-string)
		 ";k" (number->string session-id)
		 ";c" (number->string continuation-counter)
		 "-" (number->string continuation-id)))

;; Return various parts of RESUME-URL
(define (resume-url-session-id resume-url)
  (receive (session-id continuation-id)
      (resume-url-ids resume-url)
    session-id))

(define (resume-url-continuation-id resume-url)
  (receive (session-id continuation-id)
      (resume-url-ids resume-url)
    continuation-id))

(define (resume-url-ids resume-url)
  (let ((match (regexp-search *resume-url-regexp* 
			      (file-name-nondirectory resume-url))))
    (if match
	(values (string->number (match:substring match 2))
		(string->number (match:substring match 3)))
	(values #f #f))))

(define (resume-url-surflet-name resume-url)
  (let ((match (regexp-search *resume-url-regexp* resume-url)))
    (if match
	(match:substring match 1)
	(values #f #f))))

(define (resume-url? resume-url)
  (regexp-search? *resume-url-regexp* resume-url))

(define (bad-surflet-error-response s-req path-string condition)
  (make-error-response 
   (status-code internal-error) 
   (surflet-request-request s-req)
   (format #f "Error in SUrflet ~s." path-string)
   condition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record types

;;; SESSION: session-table entry for every new request on a surflet page
(define-record-type session :session
  (make-session surflet-name
                continuation-table continuation-table-lock
                continuation-counter
                session-data
                lifetime)
  session?
  (surflet-name real-session-surflet-name)
  (continuation-table session-continuation-table)
  (continuation-table-lock session-continuation-table-lock)
  (continuation-counter session-continuation-counter)
  (session-data session-session-data set-session-session-data!)
  (lifetime really-session-lifetime really-set-session-lifetime!))

(define (session-surflet-name session-or-session-id)
  (if (session? session-or-session-id)
      (real-session-surflet-name session-or-session-id)
      (let ((session (session-lookup session-or-session-id)))
	(if session
	    (real-session-surflet-name session)
	    (error "No such session / Session no longer alive." 
		   session-or-session-id)))))

(define (session-session-id session-id) session-id)

;;; INSTANCE: Every request corresponds to an instance.
(define-record-type instance :instance
  (make-instance session-id)
  instance?
  (session-id really-instance-session-id 
	       set-instance-session-id!))

;;; OPTIONS: options for the surflet-handler
(define-record-type surflet-options :suflet-options
  (really-make-surflet-options surflet-path cache-surflets? 
			       session-lifetime make-session-timeout-text)
  surflet-options?
  (surflet-path surflet-options-surflet-path set-surflet-options-surflet-path!)
  (cache-surflets? surflet-options-cache-surflets? set-surflet-options-cache-surflets?!)
  ;; session lifetime is in seconds
  (session-lifetime surflet-options-session-lifetime set-surflet-options-session-lifetime!)
  (make-session-timeout-text surflet-options-make-session-timeout-text
                             set-surflet-options-make-session-timeout-text!))

(define (default-make-session-timeout-text start-url)
  (format #f 
"<br>
<p>There may be several reasons, why your request for a SUrflet was denied:
<ul>
<li>The SUrflet does not accept any requests any more.</li>
<li>The SUrflet URL has timed out.</li>
<li>You URL is illformed.</li>
</ul>
</p>
<p>In any case, you may try to restart the SUrflet from the <a href=\"~a\">beginning</a>. Your browser may also have cached an old session of this SUrflet. In this case, try to reload the page.</p>" (file-name-nondirectory start-url)))

;; Constructor with defaults.
(define (make-default-surflet-options)
  (really-make-surflet-options #f #t 600 default-make-session-timeout-text))

(define (copy-surflet-options options)
  (let ((new-options (make-default-surflet-options)))
    (set-surflet-options-surflet-path!
     new-options
     (surflet-options-surflet-path options))
    (set-surflet-options-cache-surflets?!
     new-options
     (surflet-options-cache-surflets? options))
    (set-surflet-options-session-lifetime!
     new-options
     (surflet-options-session-lifetime options))
    (set-surflet-options-make-session-timeout-text!
     new-options
     (surflet-options-make-session-timeout-text options))
    new-options))

(define (make-surflet-options-transformer set-option!)
  (lambda (new-value . stuff)
    (let ((new-options (if (not (null? stuff))
			   (copy-surflet-options (car stuff))
			   (make-default-surflet-options))))
      (set-option! new-options new-value)
      new-options)))

(define (make-surflet-options . stuff)
  (let loop ((options (make-default-surflet-options))
	     (stuff stuff))
    (if (null? stuff)
	options
	(let* ((transformer (car stuff))
	       (value (cadr stuff)))
	  (loop (transformer value options)
		(cddr stuff))))))

(define with-surflet-path 
  (make-surflet-options-transformer
   set-surflet-options-surflet-path!))
(define with-cache-surflets?
  (make-surflet-options-transformer
   set-surflet-options-cache-surflets?!))
(define with-session-lifetime
  (make-surflet-options-transformer
   set-surflet-options-session-lifetime!))
(define with-make-session-timeout-text
  (make-surflet-options-transformer
   set-surflet-options-make-session-timeout-text!))

;; Selectors for *options* (preserved-thread-fluid)
(define (make-fluid-selector selector)
  (lambda () (selector (thread-fluid *options*))))
(define (make-fluid-setter setter)
  (lambda (value)
    (setter (thread-fluid *options*) value)))
(define options-surflet-path
  (make-fluid-selector surflet-options-surflet-path))
(define options-cache-surflets? 
  (make-fluid-selector surflet-options-cache-surflets?))
(define options-session-lifetime 
  (make-fluid-selector surflet-options-session-lifetime))
(define options-make-session-timeout-text
  (make-fluid-selector surflet-options-make-session-timeout-text))
(define set-options-surflet-path!
  (make-fluid-setter set-surflet-options-surflet-path!))
(define set-options-cache-surflets?! 
  (make-fluid-setter set-surflet-options-cache-surflets?!))
(define set-options-session-lifetime! 
  (make-fluid-setter set-surflet-options-session-lifetime!))
(define set-options-make-session-timeout-text
  (make-fluid-setter set-surflet-options-make-session-timeout-text!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RANDOM SOURCE

(define random
  (let* ((source  (make-random-source))
	 (random-integer (begin
			   (random-source-randomize! source)
			   (random-source-make-integers source))))
    (lambda ()
      (random-integer 1073741824))))	; I hope, 1+ billion is enough....

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUGGING

(define (debug fmt . args)
  (if *debug*
      (format #t "DEBUG: ~?~%" fmt args)
      (force-output)))


;;; EOF
;;; Local Variables:
;;; buffer-tag-table: "../../TAGS"
;;; End::

