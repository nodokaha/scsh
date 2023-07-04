(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	surflets/error
	profiling
	handle-fatal-error
	(subset srfi-1 (iota))
	define-record-types
	locks
	let-opt
	receiving
	(subset primitives (add-finalizer!))
	)
  (begin

    ;; This uses the filesystem heavily to not influence the
    ;; profiling. Note to get the resulting picture, gnuplot must be
    ;; installed.

    (define-record-type state :state
      (make-state file-name file-names-to-delete counter)
      state?
      (file-name state:file-name set-state:file-name!)
      (file-names-to-delete state:file-names-to-delete set-state:file-names-to-delete!)
      (counter state:counter set-state:counter!))
    
    (define (state-file-name)
      (state:file-name (get-session-data)))
    (define (state-file-names-to-delete)
      (state:file-names-to-delete (get-session-data)))
    (define (state-counter)
      (state:counter (get-session-data)))

    ;; Leave this global. Servers are running on a single system.
    (define gnuplot #f)			;; Set in main.
    (define convert #f)
    (define use-convert? #f)
    (define lock (make-lock))

    (define (profile req . maybe-update-text)
      (let* ((update-text (:optional maybe-update-text ""))
	     (gnuplot-input-field (make-text-field gnuplot '(@ (size 20))))
	     (gnuplot-change-button (make-submit-button "Change"))
	     (convert-check-box (make-checkbox use-convert?))
	     (convert-input-field (make-text-field convert '(@ (size 20))))
	     (convert-change-button (make-submit-button "Change"))
	     (new-profile-address (make-address))
	     (result-address (make-address))
	     (reset-address (make-address))
	     (return-address (make-address))
	     (reset-return-address (make-address))
	     (req 
	      (send-html/suspend
	       (lambda (new-url)
		 `(html 
		   (title "SUrflet Administration -- Profiling")
		   (body (h1 "SUrflet Administration")
			 (h2 "Profiling")
			 (p "Note: The operations performable via this interface take a while depending on the speed of the machine the server is running. Please be patient.")
			 ,(emph update-text)
			 (p "Currently, there are " ,(state-counter) " profiles saved.")
			 (ul
			  (li (url ,(new-profile-address new-url)
				   "Create new profile"))
			  (li (url ,(result-address new-url)
				   "Show profile results")
			      (br)
			      (surflet-form
			       ,new-url
			       POST
			       (table 
				(@ (border 0))
				(thead)
				(tfoot)
				(tbody 
				 (@ (valign "top"))
				 (tr
				  (td)
				  (td "This uses " (var "gnuplot") " that is searched at ")
				  (td ,(executable-input gnuplot-input-field
							 gnuplot
							 gnuplot-change-button)))
				 (tr
				  (td ,convert-check-box)
				  (td "This uses " (var "convert") " that is searched at ")
				  (td ,(executable-input convert-input-field
							 convert
							 convert-change-button)))
				 ))))
			  (li (url ,(reset-address new-url)
				   "Delete files and reset profile state.")))
			 (hr)
			 (url ,(return-address new-url) 
			      "Return to administration menu leaving files and state untouched.")
			 (br)
			 (url ,(reset-return-address new-url)
			      "Return to administration menu removing files and reseting state.")
			 (br)
			 (url "/" "Return to main menu."))))))
	     (bindings (get-bindings req)))
	(cond
	 ((returned-via? new-profile-address bindings)
	  (new-profile req))
	 ((returned-via? result-address bindings)
	  (if (zero? (state-counter))
	      (profile req "Now profiles created, currently. Select 'Create new profile' to create one.")
	      (result req)))
	 ((returned-via? reset-address bindings)
	  (reset req))
	 ((returned-via? return-address bindings)
	  (return-to-main-page req))
	 ((returned-via? reset-return-address bindings)
	  (reset-and-return-to-main-page req))
	 ((returned-via? gnuplot-change-button bindings)
	  (let ((new-gnuplot-location (input-field-value gnuplot-input-field bindings)))
	    (if (executable? new-gnuplot-location)
		(begin
		  (set! gnuplot new-gnuplot-location)
		  (profile req (format #f "Gnuplot is now searched at ~a." gnuplot)))
		(profile req "Please enter a file name of an existing executable."))))
	 ((returned-via? convert-change-button bindings)
	  (let ((new-use-convert? (input-field-value convert-check-box bindings))
		(new-convert-location (input-field-value convert-input-field bindings)))
	    (if (equal? use-convert? new-use-convert?)
		(if (executable? new-convert-location)
		    (begin
		      (set! convert new-convert-location)
		      (profile req (format #f "Convert is now searched at ~a." convert)))
		    (profile req "Please enter a file name of an existing executable."))
		(if (equal? new-convert-location convert)
		    (begin
		      (set! use-convert? new-use-convert?)
		      (profile req (format #f "Convert is ~a used now." 
					   (if use-convert? "" "not"))))
		    (begin
		      (if (executable? new-convert-location)
			  (begin
			    (set! use-convert? new-use-convert?)
			    (set! convert new-convert-location)
			    (profile req (format #f "Convert (at ~a) is ~a used now."
						 convert
						 (if use-convert? "" "not"))))
			  (profile req (format #f "Please enter a file name of an existing executable."))))))))
	 (else
	  (error "Unexpected choice.")))))
    
    (define (executable? file-name)
      (and file-name
	   (file-executable? file-name)))

    (define (new-profile req)
      (let ((state (get-session-data)))
	(format #t "profiling...~%")
	(obtain-lock lock)
	(profile-space (state:file-name state))
	(release-lock lock)
	(format #t "profile recorded in ~s~%" (state:file-name state))
	(set-state:counter! state (+ 1 (state:counter state)))
	(profile req (format #f "Profile #~a generated" (state:counter state)))))

    (define (result req)
      (let ((results (profile-results (state-file-name)))
	    (gnuplot-data-file-name (create-temp-file "surflet-profiling.data"))
	    (gnuplot-picture-name (create-empty-picture-file 
				"../img/surflet-profiling.picture"
				".pbm"))
	    (convert-picture-name (create-empty-picture-file
				   "../img/surflet-profiling.picture"
				   ".png"))
	    (get-total-bytes (lambda (space-info)
			       (total-bytes (space-info-total space-info))))
	    (return-address (make-address))
	    (reset-return-address (make-address)))
	(write-gnuplot-data-file gnuplot-data-file-name 
				 get-total-bytes
				 results)
	(let* ((gnuplot-status 
		(run (,gnuplot -)
		    (<< ,(format #f "set terminal pbm color
set output '~a'
set size 0.7,0.7
plot '~a' title 'SUrflet Profiling ~a' with lines"
				 gnuplot-picture-name
				 gnuplot-data-file-name
				 (format-date "~c" (date))
				 ))))
	      (convert-status (and use-convert?
				   (zero? gnuplot-status)
				   (run (,convert ,gnuplot-picture-name
						  ,convert-picture-name)))))
	  (delete-file gnuplot-data-file-name)
	  (add-file-name-to-delete! gnuplot-picture-name)
	  (if (and use-convert?
		   (not (zero? convert-status)))
		   (add-file-name-to-delete! convert-picture-name))
	  (let* ((req (show-results gnuplot-status gnuplot-picture-name
				    convert-status convert-picture-name
				    get-total-bytes results 
				    return-address reset-return-address))
		 (bindings (get-bindings req)))
	    (cond
	     ((returned-via? return-address bindings)
	      (profile req "You may continue to make profiles."))
	     ((returned-via? reset-return-address bindings)
	      (reset-and-return-to-main-page req))
	     (else
	      (error "unexpected return address")))))))

    (define create-empty-picture-file
      (let ((lock (make-lock)))
	(lambda (file-prefix file-suffix)
	  (obtain-lock lock)
	  (let* ((tmp-file-name  (create-temp-file  file-prefix))
		 (picture-file-name (string-append tmp-file-name file-suffix)))
	    (rename-file tmp-file-name picture-file-name)
	    (release-lock lock)
	    picture-file-name))))
    
    (define (show-results gnuplot-status gnuplot-picture-name
			  convert-status convert-picture-name
			  get-total-bytes results
			  return-address reset-return-address)
      (send-html/suspend
       (lambda (new-url)
	 `(html 
	   (title "SUrflet Administration -- Profiling Results")
	   (body
	    (h1 "SUrflet-Administration")
	    (h2 "Profiling Results")
	    (h3 "Picture")
	    ,(if (zero? gnuplot-status)
		 (if use-convert?
		     (if (zero? convert-status)
			 `(image (@ (src ,convert-picture-name)))
			 `(p "An error occured while generating the profiling results"
			     " chart with convert (" ,convert ")."
			     " Anyway, you can download the "
			     (url ,gnuplot-picture-name "raw profiling chart") "."))
		       `(url ,gnuplot-picture-name "Profiling chart."))
		 `(p "An error occured while generating the profiling results picture."
		     (br)
		     "Are you sure, you have " (q "gnuplot") 
		     " installed at " (q ,gnuplot) "?"))
	    (hr)
	    (h3 "Data")
	    (table
	     (tr (th "#") (th "Total bytes occupied"))
	     ,@(map (lambda (num space-info)
		      `(tr (td ,(+ 1 num))
			   (td ,(get-total-bytes space-info))))
		    (iota (length results))
		    results))
	    (hr)
	    (p (url ,(return-address new-url) "Return to previous page") (br)
	       (url ,(reset-return-address new-url) 
		    "Delete files, reset state and return to main menu.")))))))

    (define (reset req)
      (reset-profiling-state!)
      (profile req "Profiling state reseted."))

    (define (add-file-name-to-delete! file-name)
      (let ((state (get-session-data)))
	(set-state:file-names-to-delete! 
	 state 
	 (cons file-name
	       (state:file-names-to-delete state)))))

    (define (delete-files state)
      (let ((file-names-to-delete (state:file-names-to-delete state)))
	(if file-names-to-delete
	    (for-each delete-filesys-object file-names-to-delete))))

    (define (reset-profiling-state!)
      (let ((state (get-session-data)))
	(set-state:counter! state 0)
	(delete-files state)
	(set-state:file-name! state 
			      (absolute-file-name (create-temp-file "surflet-profiling")))
	(set-state:file-names-to-delete! state
					 (list (state:file-name state)))))
    
    (define (reset-and-return-to-main-page req)
      ;; Overhead included :-|
      (reset-profiling-state!)
      (delete-files (get-session-data))
      (return-to-main-page req))

    (define (return-to-main-page req)
      (send-error (status-code moved-perm) req
		  "admin.scm" "admin.scm"))

    (define (main req)
      ;; We'll fill this out soon.
      (set! gnuplot (search-executable "gnuplot"))
      (if (string=? gnuplot "")
	  (begin
	    (set! use-convert? #f)
	    (set! convert ""))
	  (begin
	    (set! convert (search-executable "convert"))
	    (if (string=? convert "")
		(set! use-convert? #f)
		(set! use-convert? #t))))
      (set-session-data! (make-state #f #f 0))
      (reset-profiling-state!)
      ;; Remove state files if user did not do it.
      (add-finalizer! (get-session-data) delete-files)
      (profile req))

    (define (search-executable exec-name)
      (with-fatal-error-handler
       ;; If `which' is unavailable, return nothing.
       (lambda (condition decline) "")
       (receive (status ports) (run/collecting (1) (which ,exec-name))
	 (if (zero? status)
	     (read-line ports)
	     ""))))
    
    (define (emph text)
      `(font (@ (color "red")) ,text))

    (define (executable-input input-field exec-name change-button)
      `(table 
	(@ (border 0))
	(tr (td ,input-field) (td ,change-button))
	,(if (executable? exec-name)
	     #f
	     `(tr (td (@ (colspan 2))
		      ,(emph "Note: There is no executable."))))))

    ))