(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	surflets/callbacks		;make-callback
	surflets/outdaters
	surflets/error
	surflet-handler/admin
	handle-fatal-error
	let-opt
	srfi-1				;filter-map
	sort
	)
  (begin

    (define remove-surflet-path
      (let ((regexp (rx ,(file-name-as-directory (options-surflet-path)) 
			(submatch (* any)))))
	(lambda (file-name)
	  (let ((match (regexp-search regexp file-name)))
	    (if match
		(match:substring match 1)
		file-name)))))

    ;; returns two values: an action to perform out of ACTIONS and a
    ;; list of selected elements out of TABLE-ELEMENTS.
    (define (select-table title header header-row 
			  table-elements selector actions footer)
      (let* ((checkboxes (map (lambda (_) 
				(make-checkbox)) 
			      table-elements))
	     (action-title "Choose an action")
	     (select (make-select (cons action-title actions) 
					      '(@ (size 1))))
	     (req
	      (send-html/suspend
	       (lambda (new-url)
		 `(html 
		   (title ,title)
		   (body
		    ,header
		    (surflet-form
		     ,new-url
		     (table
		      ,@(cons '(th) header-row)
		      ,@(map (lambda (checkbox table-element)
			       `(tr 
				 (td ,checkbox) 
				 ,@(selector table-element)))
			     checkboxes
			     table-elements))
		     (p ,select
			,(make-submit-button "Do it")))
		    ,footer)))))
	     (bindings (get-bindings req))
	     (action (input-field-value select bindings)))

	(if (string=? action action-title)
	    (select-table title header header-row table-elements selector actions footer)
	    (values
	     action
	     (filter-map (lambda (checkbox table-element)
			   (if (input-field-value checkbox bindings)
			       table-element
			       #f))
			 checkboxes
			 table-elements)))))

    (define (unload-surflets outdated? surflet-names)
      (if-outdated outdated?
	  (show-outdated (make-callback surflets))
	  (for-each unload-surflet surflet-names)))

    (define (no-surflets)
      `(p "Currently, there are no SUrflets loaded " 
	  (url ,(make-callback surflets) "(reload).")))

    (define (surflets req . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (loaded-surflets (sort-list! (get-loaded-surflets) string<?))
	     (outdated? (make-outdater))
	     (title "SUrflet-Administration -- SUrflets")
	     (header `((h1 "SUrflet Administration")
		       (h2 "SUrflets")
		       (p (font (@ (color "red")) ,update-text))))
	     (footer `((hr)
		       (url ,(make-callback return-to-main-page) "Return to main page")))
	     (actions '("unload" "unload all" "view sessions")))
	(if (null? loaded-surflets)
	    (send-html `(html (title ,title) (body ,header ,(no-surflets) ,footer)))
	    (receive (action selected-surflets)
		(select-table title	                 ; title
			      header	                 ; header
			      '((th "Name"))             ; table-header
			      loaded-surflets            ; list of elements
			      (lambda (surflet)          ; selector
				`((td
				   ,(remove-surflet-path surflet))))
			      actions                    ; actions to perform
			      (cons                      ; footer
			       `(p "Note that unloading the SUrflet does not imply "
				   "the unloading of sessions of this SUrflet." 
				   "This can be done on the " 
				   (url ,(make-callback sessions) 
					"sessions adminstration page."))
			       footer))
	      (if (null? selected-surflets)
		  (surflets 'no-req "You must choose at least one element.")
		  (cond 
		   ((string=? action "unload")
		    (unload-surflets outdated? selected-surflets)
		    (surflets 'no-req "SUrflets unloaded."))
		   ((string=? action "unload all")
		    (unload-surflets outdated? loaded-surflets)
		    (surflets 'no-req "SUrflets unloaded."))
		   ((string=? action "view sessions")
		    (format #t "~s~%" selected-surflets)
		    (let* ((path-stripped-selected-surflets
			    (map remove-surflet-path selected-surflets))
			   (selected-sessions
			    (filter (lambda (session-pair) 
				      (member (session-surflet-name (cdr session-pair))
					      path-stripped-selected-surflets))
				    (get-sessions))))
		      ;; this does not return
		      (real-sessions (sort-list! selected-sessions 
						  session-surflet-name<?)
				      "")))
		   (else
		    (error "unknown action" action))))))))

    (define (session-surflet-name<? entry1 entry2)
      (let ((name1 (session-surflet-name (cdr entry1)))
	    (name2 (session-surflet-name (cdr entry2))))
	;; handle multiple session names
	(if (string=? name1 name2)
	    (session-id<? entry1 entry2)
	    (string<? name1 name2))))
    (define (session-id<? entry1 entry2)
      ;; there are no multiple session-ids
      (< (car entry1) (car entry2)))
    (define (session-id>? entry1 entry2)
      (session-id<? entry2 entry1))
    (define (session-surflet-name>? entry1 entry2)
      (session-surflet-name<? entry2 entry1))

    (define (no-current-sessions)
      ;; Avoid using send/suspend in this context as there
      ;; are no sessions available any more.
      '(p "Currently, there are no sessions, "
	  "i.e. the administration SUrflet is no longer running. "
	  ;; Can't use callback here, as there are no valid sessions left.
	  (url "admin.scm" "Go back to main page.")))

    (define (sessions req . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (current-sessions (sort-list! (get-sessions) session-surflet-name<?)))
	(real-sessions current-sessions update-text)))

    (define (real-sessions current-sessions update-text)
      (let ((outdated? (make-outdater))
	     (title  "SUrflet Adminstration - Sessions")
	     (header `((h1 "SUrflet Administration")
		       (h2 "Sessions")
		       (p (font (@ (color "red")) ,update-text))))
	     (footer `((hr)
		       (url ,(make-callback return-to-main-page) "Return to main page")))
	     (actions '("kill"
			"adjust timeout" 
			"view continuations"))
	     (sessions-callback (make-callback sessions)))
	(if (null? current-sessions)
	    (send-html `(html (title ,title) 
			      (body ,@header ,(no-current-sessions) ,footer)))
	    (receive (action selected-sessions)
		(select-table title 
			      header
			      `((th "SUrflet Name") (th "Session-Id"))
			      current-sessions
			      (lambda (session-pair)
				(let ((session-id (car session-pair))
				      (session-entry (cdr session-pair)))
				  `((td ,(session-surflet-name session-entry))
				    (td ,session-id))))
			      actions
			      footer)
	      (let ((new-update-text
		     (cond
		      ((string=? action "kill")
		       (if-outdated outdated?
			   (show-outdated sessions-callback)
			   (for-each delete-session! 
				     (map car selected-sessions)))
		       "Sessions killed.")
		      ((string=? action "adjust timeout")
		       (if-outdated outdated?
			   (show-outdated sessions-callback)
			   (for-each session-adjust-timeout! 
				     (map car selected-sessions)))
		       "Sessions killed.")
		      ((string=? action "view continuations")
		       (if-outdated outdated?
			   (show-outdated sessions-callback)
			   (if (zero? (length selected-sessions))
			       "You must choose at least one session."
			       ;; this does not return
			       (continuations selected-sessions))))
		      (else
		       (error "unknown action" action)))))
		(sessions 'no-req new-update-text))))))
    


    (define (no-current-continuations session)
      `((p "Currently, there are no continuations for this session. ")
	(p "You may " (url ,(make-callback 
			     (lambda (req) (continuations (list session))))
			   "reload")
	   " this page or go back to the "
	   (url ,(make-callback sessions) "session table overview."))))

    (define (no-more-than-one-session title header1)
      (send-html 
       `(html (title ,title) 
	      (body (h1 "SUrflet Administration")
		    (p "Currently, you may only view the continuations of "
		       "one session at a time. This will be changed in "
		       "future revisions. Sorry for any inconvenience.")
		    (p "You may choose to go back to the " 
		       (url ,(make-callback sessions) 
			    "sessions administration page")
		       " where you can choose one session.")))))
      
    (define (continuation-id<? entry1 entry2)
      (< (car entry1) (car entry2)))

    (define (continuations sessions  . maybe-update-text)
      (let ((title "SUrflet Adminstration - Continuations")
	    (header1 '(h1 "SUrflet Administration")))
	(if (not (= 1 (length sessions)))
	    (no-more-than-one-session title header1)
	    (let* ((session-pair (car sessions))
		   (session-id (car session-pair))
		   (session-entry (cdr session-pair))
		   (update-text (:optional maybe-update-text "")))
	      (let ((current-continuations 
		     (sort-list! (get-continuations session-id)
				 continuation-id<?))
		    (outdated? (make-outdater))
		    
		    (header (cons header1
				  `((h2 "Continuations of " ,session-id)
				    (p "(belongs to the SUrflet '" 
				       ,(session-surflet-name session-entry) "')")
				    (p (font (@ (color "red")) ,update-text)))))
		    (footer 
		     `((hr)
		       (url ,(make-callback sessions) 
			    "Return to sessions page.") (br)
		       (url ,(make-callback return-to-main-page)
			    "Return to main page.")))
		    (actions '("delete" "delete all"))
		    (continuations-callback 
		     (make-callback (lambda (req)
				      (continuations sessions)))))
		(if (null? current-continuations)
		    (send-html `(html (title ,title) 
				      (body ,header 
					    ,(no-current-continuations session-pair)
					    ,footer)))
		    (receive (action selected-continuations)
			(select-table title
				      header
				      '((th "Continuation-Id"))
				      current-continuations
				      (lambda (continuation-pair)
					(let ((continuation-id (car continuation-pair)))
					  `((td ,continuation-id))))
				      actions
				      footer)
		      (cond 
		       ((string=? action "delete")
			(delete-continuations outdated? continuations-callback 
					      session-id selected-continuations))
		       ((string=? action "delete all")
			(delete-continuations outdated? continuations-callback
					      session-id current-continuations))
		       (else
			(error "unknown action" action)))
		      (continuations sessions "Deleted."))))))))

  (define (delete-continuations outdated? continuations-callback
				session-id continuations)
    (if-outdated outdated?
	(show-outdated continuations-callback)
	;; Do it this way to easily expand to more sessions in the
	;; future.
	(for-each delete-continuation! 
		  (make-list (length continuations)
			     session-id)
		  (map car continuations))))

    (define (return-to-main-page req)
      (send-error (status-code moved-perm) req
		  "admin.scm" "admin.scm"))
    
    (define (main req)
      (surflets req))
    
    ))