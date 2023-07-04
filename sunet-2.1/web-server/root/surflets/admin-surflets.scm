(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	surflets/callbacks		;make-callback
	surflets/outdaters
	surflets/ids
	surflets/error
	surflet-handler/admin
	handle-fatal-error
	let-opt
	srfi-1				;filter-map, last
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
      (let* ((checkboxes (map make-annotated-checkbox
			      table-elements))
	     (select (make-annotated-select 
		      actions #f '(@ (size 1))))
	     (req
	      (send-html/suspend
	       (lambda (new-url)
		 `(html 
		   (title ,title)
		   (body
		    ,header
		    (surflet-form
		     ,new-url
		     POST
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
	     (selected (filter-map (lambda (checkbox)
				  (input-field-value checkbox bindings))
				checkboxes))
	     (action (input-field-value select bindings)))
	(action req selected)))

    (define (unload-surflets outdated?)
      (lambda (req surflet-names)
	(if (null? surflet-names)
	    (show-surflets req "You must choose at least one element.")
	    (if-outdated outdated?
		(show-outdated (make-callback show-surflets))
		(begin
		  (for-each unload-surflet surflet-names)
		  (show-surflets req "SUrflets unloaded."))))))
      
    (define (no-surflets callback)
      `(p "Currently, there are no SUrflets loaded " 
	  (url ,(callback show-surflets) "(reload)")
	  ", but there may be "
	  (url ,(callback show-sessions) "sessions")
	  " you want to administer."))

    (define (choose-an-action show)
      (lambda (req _)
	(show req "Choose an action.")))

    (define (show-surflets req . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (loaded-surflets (sort-list! (get-loaded-surflets) string<?))
	     (outdated? (make-outdater))
	     (callback (make-annotated-callback callback-function))
	     (title "SUrflet-Administration -- SUrflets")
	     (header `((h1 "SUrflet Administration")
		       (h2 "SUrflets")
		       (p (font (@ (color "red")) ,update-text))))
	     (footer `((hr)
		       (url ,(callback return-to-main-page)
			    "Return to administration menu.")
		       (br)
		       (url "/" "Return to main menu."))))
	(if (null? loaded-surflets)
	    (send-html `(html (title ,title) 
			      (body ,header ,(no-surflets callback) ,footer)))
	    (let ((actions 
		   (map (lambda (action-pair)
			  (make-annotated-select-option
			   (car action-pair)
			   (cdr action-pair)))
			`(("Choose an action" . ,(choose-an-action show-surflets))
			  ("unload" . ,(unload-surflets outdated?))
			  ("unload all" . ,(lambda (req _)
					     ((unload-surflets outdated?) 
					      req loaded-surflets)))))))
	      (select-table title	                 ; title
			    header	                 ; header
			    '((th "Name"))             ; table-header
			    loaded-surflets            ; list of elements
			    (lambda (surflet)          ; selector
			      `((td
				 ,(remove-surflet-path surflet))))
			    actions                    ; actions to perform
			    (cons                      ; footer
			     `(p "Note that unloading the SUrflets does not imply "
				 "the unloading of sessions of this SUrflet. " (br)
				 "This can be done on the " 
				 (url ,(callback show-sessions) 
				      "sessions adminstration page."))
			     footer))))))

    (define (session-surflet-name<? session1 session2)
      (let ((name1 (session-surflet-name session1))
	    (name2 (session-surflet-name session2)))
	;; handle multiple session names
	(if (string=? name1 name2)
	    (session-id<? session1 session2)
	    (string<? name1 name2))))
    (define (session-id<? session1 session2)
      ;; there are no multiple session-ids
      (< (session-session-id session1)
	 (session-session-id session2)))
    (define (session-surflet-name>? session1 session2)
      (session-surflet-name<? session2 session1))
    (define (session-id>? session1 session2)
      (session-id<? session2 session1))

    (define (no-current-sessions)
      ;; Avoid using send/suspend in this context as there
      ;; are no sessions available any more.
      '(p "Currently, there are no sessions, "
	  "i.e. the administration SUrflet is no longer running. "
	  ;; Can't use callback here, as there are no valid sessions left.
	  (url "admin.scm" "Go back to main page.")))

    (define (show-sessions req . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (current-sessions (sort-list! (get-sessions) session-surflet-name<?)))
	(real-sessions current-sessions update-text 
		       (my-session-id req))))

    (define (kill-sessions outdated? sessions-callback)
      (lambda (req selected-sessions)
	(if-outdated outdated?
	    (show-outdated sessions-callback)
	    (for-each delete-session! 
		      selected-sessions))
	(show-sessions req "Sessions killed.")))

    (define (adjust-session-timeout outdated? sessions-callback)
      (lambda (req selected-sessions)
	(if-outdated outdated?
	    (show-outdated sessions-callback)
	    (for-each session-adjust-timeout! 
		      selected-sessions))
	(show-sessions req "Timeout adjusted.")))

    (define (view-continuations outdated? sessions-callback)
      (lambda (req selected-sessions)
	(if-outdated outdated?
	    (show-outdated sessions-callback)
	    (if (zero? (length selected-sessions))
		(show-sessions req "You must choose at least one session.")
		;; this does not return
		(show-continuations req selected-sessions)))))

    (define (real-sessions current-sessions update-text this-session-id)
      (let* ((outdated? (make-outdater))
	     (callback (make-annotated-callback callback-function))
	     (title  "SUrflet Adminstration - Sessions")
	     (header `((h1 "SUrflet Administration")
		       (h2 "Sessions")
		       (p (font (@ (color "red")) ,update-text))))
	     (footer `(,(if (not (null? current-sessions))
			    `(p "Be careful not to kill this adminstration's "
				"session (id: " ,this-session-id ").")
			    #f)
		       (hr)
		       (url ,(callback show-surflets) 
			    "Return to SUrflets menu.") 
		       (br) (url ,(callback return-to-main-page)
				 "Return to administration menu.")
		       (br) (url "/" "Return to main menu.")))
	     (sessions-callback (callback show-sessions)))
	(if (null? current-sessions)
	    (send-html `(html (title ,title) 
			      (body ,@header ,(no-current-sessions) ,footer)))
	    (let ((actions 
		   (map (lambda (action-pair)
			  (make-annotated-select-option
			   (car action-pair)
			   (cdr action-pair)))
			`(("Choose an action" . ,(choose-an-action show-sessions))
			  ("kill" . ,(kill-sessions outdated?  sessions-callback))
			  ("adjust timeout" . 
			   ,(adjust-session-timeout outdated?
						    sessions-callback))
			  ("view continuations" . 
			   ,(view-continuations outdated? 
						sessions-callback))))))
	      (select-table title 
			    header
			    `((th "SUrflet Name") (th "Session-Id"))
			    current-sessions
			    (lambda (session)
			      `((td ,(session-surflet-name session))
				(td (@ (align "right")) 
				    ,(session-session-id session))))
			    actions
			    footer)))))

    (define (no-current-continuations callback session req)
      `((p "Currently, there are no continuations for this session. ")
	(p "You may " (url ,(callback show-continuations (list session))
			   "reload")
	   " this page or go back to the "
	   (url ,(callback show-sessions) "session table overview."))))

    (define (no-more-than-one-session title header1 sessions req)
      (let* ((address (make-annotated-address))
	     (req (send-html/suspend
		   (lambda (k-url)
		     `(html 
		       (title ,title) 
		       (body 
			(h1 "SUrflet Administration")
			(p "Currently, you may only view the continuations of "
			   "one session at a time. This will be changed in "
			   "future revisions. Sorry for any inconvenience.")
			(p "You may choose to go back to the " 
			   (url ,(make-callback show-sessions) 
				"sessions administration page")
			   " where you can select one session"
			   " or select one session from your chosen sessions:" (br)
			   (ul
			    ,@(map 
			       (lambda (session)
				 `(li (url ,(address k-url session)
					   ,(session-surflet-name session)
					   " (" ,(session-session-id session) ")")))
				   sessions))))))))
	     (bindings (get-bindings req))
	     (chosen-session (returned-via address bindings)))
	(show-continuations req (list chosen-session))))
      
    (define (continuation-id<? cont1 cont2)
      (< (continuation-id cont1) 
	 (continuation-id cont2)))

    (define (show-continuations req sessions . maybe-update-text)
      (let ((title "SUrflet Adminstration - Continuations")
	    (header1 '(h1 "SUrflet Administration")))
	(if (not (= 1 (length sessions)))
	    (no-more-than-one-session title header1 sessions req)
	    (let* ((session (car sessions))
		   (session-id (session-session-id session))
		   (this-continuation-id (my-continuation-id req))
		   (update-text (:optional maybe-update-text ""))
		   (current-continuations 
		    (sort-list! (get-continuations session-id)
				continuation-id<?))
		   (outdated? (make-outdater))
		   (callback (make-annotated-callback callback-function))
		   (header (cons header1
				 `((h2 "Continuations of " ,session-id)
				   (p "(belongs to the SUrflet '" 
				      ,(session-surflet-name session) "')")
				   (p (font (@ (color "red")) ,update-text)))))
		   (footer 
		    `(,(if (not (null? current-continuations))
			   `(p "Be careful not to delete this adminstration's "
			       "continuation (id: " ,this-continuation-id ").")
			   #f)
		      (hr)
		      (url ,(callback show-sessions) 
			   "Return to sessions menu.")
		      (br) (url ,(callback show-surflets)
				"Return to SUrflets menu.")
		      (br) (url ,(callback return-to-main-page) 
				"Return to administration menu.")
		      (br) (url "/" "Return to main menu.")))
		   (continuations-callback (callback show-continuations sessions)))
	      (if (null? current-continuations)
		  (send-html 
		   `(html (title ,title) 
			  (body ,header 
				,(no-current-continuations callback session req)
				,footer)))
		  (let ((actions
			 (map (lambda (action-pair)
				(make-annotated-select-option
				 (car action-pair)
				 (cdr action-pair)))
			      `(("Choose an action" . 
				 ,(lambda (req _)
				    (show-continuations req sessions
							"Choose an action.")))
				("delete" . 
				 ,(lambda (req selected-continuations)
				    (delete-continuations outdated? 
							  continuations-callback 
							  selected-continuations)
				    (show-continuations req sessions
							"Deleted.")))
				("delete all" . 
				 ,(lambda (req _)
				    (delete-continuations outdated? 
							  continuations-callback
							  current-continuations)
				    (show-continuations req sessions
							"Deleted.")))))))
		    (select-table title
				  header
				  '((th "Continuation-Id"))
				  current-continuations
				  (lambda (continuation)
				    `((td (@ (align "right")) 
					  ,(continuation-id continuation))))
				  actions
				  footer)))))))

  (define (delete-continuations outdated? continuations-callback
				continuations)
    (if-outdated outdated?
	(show-outdated continuations-callback)
	;; Do it this way to easily expand to more sessions in the
	;; future.
	(for-each delete-continuation! continuations)))

    (define (return-to-main-page req)
      (send-error (status-code moved-perm) req
		  "admin.scm" "admin.scm"))
    
    (define (main req)
      (show-surflets req))
    
    ))