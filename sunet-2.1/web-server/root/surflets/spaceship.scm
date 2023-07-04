(define-structure surflet surflet-interface
  (open scheme-with-scsh
	surflets
	let-opt
	receiving
	define-record-types
	(subset srfi-13 (string-downcase string-join))
	(subset srfi-1 (find filter-map split-at remove))
	sunet-utilities
	surflet-requests)
(begin

;;; Spaceship components
;;; Size (Class ...)
;;; Arms (Photontorpedos, Phaser)
;;; Shields
;;; Drive (Impuls, Warp)
;;; Extras (Double Casing, Trans Warp Drive, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA
(define-record-type ship-data :ship-data
  (make-ship-data class max-crew med-sections max-shuttles arm-types 
		  max-arms max-shield extras build-time)
  ship-data?
  (class ship-data-class)
  (max-crew ship-data-max-crew)
  (med-sections ship-data-med-sections)
  (max-shuttles ship-data-max-shuttles)
  (arm-types ship-data-arm-types)
  (max-arms ship-data-max-arms)
  (max-shield ship-data-max-shield)
  (extras ship-data-extras)
  (build-time ship-data-build-time))

;;; This are the orderable ships with their data. The following
;;; procedures will refer to this list to get the data for a ship
;;; class.
(define ships
  (map (lambda (data)
	 (apply make-ship-data data))
       ;; class-name	max-crew med-sections	max-shuttles
       ;;               (possible) arm-types 
       ;;		max-arms (TW)   max-shields (TJ)
       ;;		extras
       ;;		build-time (months)
       '(("Constitution" 400	14		10
			(7 torpedo2)	
			17000	729000
			(double-casing tractor shuttle-ramp)
			6)
	 ("Excelsior"	 570	14		#f
			(8 7 torpedo2)	
			41000	2106000
			(double-casing tractor transwarp)
			7)
	 ("Ambassador"	550	15		#f
			(9 8 7 torpedo1)	
			62500	4298000
			(double-casing tractor)
			8)
	 ("Galaxy"	 760	17		25
			(10 9 8 torpedo2 torpedo-M/AM)
			61200	5400000
			(double-casing tractor discus wide-angle-firing
					transporter captains-yacht
					life-maintenance) 
			10))
       ))

;;; All orderable classes extracted from ship data list.
(define classes (map ship-data-class ships))

;;; All orderable drives.
(define drives
  '("Impuls" "Warp"))

;;; All orderable arm types.
(define arm-types
  '((7 . "Phaser Type VII")  (8 . "Phaser Type VIII")
    (9 . "Phaser Type IX") (10 .  "Phaser Type X")
    (torpedo1 . "Class 1 Torpedo")
    (torpedo2 . "Photon-Torpedo-System Class 2")
    (torpedo-M/AM . "Photonen-Torpedo-System M/AM")))

;;; All orderable extras. The ship data contains a list of extras a
;;; ship class may have.
(define extras
  '((double-casing . "Double Casing")
    (tractor . "Tractor Ray")
    (shuttle-ramp . "Shuttle Ramp")
    (transwarp . "Trans Warp Drive (experimental)")
    (discus . "Detachable Discus Section")
    (wide-angle-firing . "300° Fire Angle")
    (transporter . "Extra Transporters (+35)")
    (captains-yacht . "Captain's Yacht")
    (life-maintenance . "Extended Life Maintenance System (ELMS)")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculation

;;; Main entry point. Shows a welcome message, and invites to order a
;;; spaceship.
(define (main req . update-text+class+armed?+shields?+drive)
  (let-optionals update-text+class+armed?+shields?+drive
      ((update-text #f)
       (def-class #f)
       (def-armed? #f)
       (def-shields? #f)
       (def-drive #f))
    (let* ((class-radios (make-radios
			  (checked-radio classes def-class)))
	   (drive-radios (make-radios
			  (checked-radio drives def-drive)))
	   (armed-checkbox (make-checkbox def-armed?))
	   (shield-checkbox (make-checkbox def-shields?))
	   (req (send-html/suspend
		 (lambda (new-url)
		   (generate-main-page new-url update-text
				       class-radios drive-radios 
				       armed-checkbox shield-checkbox))))
	   (bindings (get-bindings req))
	   (class (input-field-value (car class-radios) bindings))
	   (armed? (input-field-value armed-checkbox bindings))
	   (shields? (input-field-value shield-checkbox bindings))
	   (drive (input-field-value (car drive-radios) bindings)))
      (cond
       ((not class)
	(main req "Please specifiy a class." class armed? shields? drive))
       ((not drive)
	(main req "Please specifiy a drive." class armed? shields? drive))
       (else
	(specify-components req #f class armed? shields? drive))))))

;;; Step 2ff: Let the customer specify the crew size, the arming, the
;;; shields and the extras, showing a result page at the end.
(define (specify-components req update-text class armed? shields? drive)
  (receive (size med-beds)
      (get-size req class)
    (receive (weapons energy shield)
	(get-armed+shields req class armed? shields?)
      (order-page req
		  class
		  weapons energy
		  shield
		  drive
		  size
		  med-beds
		  (get-extras req class)))))

;;; Ask the customer about details for the arming and the shield of
;;; his spaceship. The selectable components are taken from the ship
;;; data list.
(define (get-armed+shields req class armed? shields? . 
			   maybe-update-text+weapons+energy+shield)
  (let-optionals maybe-update-text+weapons+energy+shield
      ((update-text #f)
       (def-weapons #f)
       (def-energy #f)
       (def-shield #f))
    (let* ((checkboxes+text
	    (and armed?
		 (map (lambda (type)
			(let ((text (cdr (assoc type arm-types))))
			  (cons (make-annotated-checkbox
				 text
				 (and def-weapons (member? text def-weapons)))
				text)))
		      (ship-data-arm-types (ship-ref class)))))
	   (energy-input (and armed?
			      (if def-energy 
				  (make-number-field def-energy)
				  (make-number-field))))
	   (shield-input (and shields?
			      (if def-shield
				  (make-number-field def-shield)
				  (make-number-field))))
	   (req (send-html/suspend
		 (lambda (new-url)
		   (generate-armed+shield-page new-url update-text
					       checkboxes+text energy-input
					       shield-input))))
	   (bindings (get-bindings req))
	   (weapons (and armed?
			 (filter-map (lambda (checkbox+text)
				       (input-field-value (car checkbox+text) bindings))
				     checkboxes+text)))
	   (energy (and armed?
			(input-field-value energy-input bindings)))
	   (shield (and shields?
			(input-field-value shield-input bindings)))
	   (complains 
	    (remove not
		    (list
		     (and armed?
			  (null? weapons)
			  need-weapons)
		     (and armed?
			  (check-bounded-number-field class energy positive-energy 
						      ship-data-max-arms 
						      arms-boundary))
		     (and shields?
			  (check-bounded-number-field class shield positive-shield 
						      ship-data-max-shield 
						      shield-boundary))))))
      (if (null? complains)
	  (values weapons energy shield)
	  (get-armed+shields req class armed? shields?
			     `(p ,@(map (lambda (complain) `(,complain (br)))
					complains))
			     weapons energy shield)))))

;;; Ask the customer about extras he want for his ship. The selectable
;;; items are taken from the ship data list.
(define (get-extras req class)
  (let* ((checkboxes+text (map (lambda (extra)
				 (cons (make-annotated-checkbox extra)
				       (cdr (assoc extra extras))))
			       (ship-data-extras (ship-ref class))))
	 (req (send-html/suspend
	       (lambda (new-url)
		 (generate-extras-page new-url class checkboxes+text))))
	 (bindings (get-bindings req)))
    (filter-map (lambda (checkbox+text)
		  (and (input-field-value (car checkbox+text) bindings)
		       (cdr checkbox+text)))
		checkboxes+text)))

;;; Show the selected components of the customers ship and ask him for
;;; ordering the whole thing (without telling him, how long this will
;;; take, of course ;-) )
(define (order-page req class weapons arms-energy shield-energy drive 
		    size med-beds extras)
  (send-html/suspend
   (lambda (new-url)
     (generate-order-page new-url class weapons arms-energy 
			  shield-energy drive size med-beds
			  extras)))
  (send-html/finish
   (generate-finish-page (calculate-build-time class weapons arms-energy 
					       shield-energy drive size
					       extras)
			 req)))

;;; This returns the number of months that are probably necessary to
;;; build the ship. The data are taken from experience of the last
;;; five years :-)
(define (calculate-build-time class weapons arms-energy shield-energy 
			      drive size extras)
  (+ (ship-data-build-time (ship-ref class))
     (if weapons
	 (+ (length weapons)
	    (if (> arms-energy 40000) 2 1))
	 0)
     (if shield-energy
	 (if (> shield-energy 2200000)
	     3
	     2)
	 0)
     4					; for impulse drive
     (if (string=? drive "Warp") 2 0)	; extra for warp drive
     (if (> size 300) 3 2)		; This includes the med-beds.
     (length extras)
     ))

;;; This asks the customer to specify how many crew members his ship
;;; will have. We only check that there is at least one crew member
;;; and the maximum crew member for a class is not exceeded.
(define (get-size req class . maybe-update-text)
  (let* ((update-text (:optional maybe-update-text #f))
	 (size-input (make-number-field))
	 (req (send-html/suspend
	       (lambda (new-url)
		 (generate-size-page new-url update-text
				     class size-input))))
	 (bindings (get-bindings req))
	 (size (input-field-value size-input bindings)))
    (if (or (not size)
	    (<= size 0))
	(get-size req class positive-size)
	(let* ((ship (ship-ref class))
	       (max-size (ship-data-max-crew ship) ))
	  (if (<= size max-size)
	      (values size (ship-data-med-sections ship))
	      (get-size req class (complain-size class max-size)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page and text generating

;;; Title of each HTML page.
(define (make-title)
  '(title "Spaceship Builder"))

;;; The following procedure do the actual HTML composing for the
;;; different steps. Nothing exciting here.

;;; Main page HTML.
(define (generate-main-page new-url update-text
			    class-radios drive-radios
			    armed-checkbox shield-checkbox)
  `(html 
    ,(make-title)
    (body
     (h1 "Welcome to the Spaceship Builder Web Site!")
     (p "Here you can build your own space ship.")
     (h2 "Step 1 -- Selecting components")
     ,(and update-text `(font (@ (color "red")) ,update-text))
     (surflet-form ,new-url
       GET
       (table
	(tr (@ (valign "top"))
	    (td "My spaceship is of class")
	    (td (table ,@(table-radios class-radios classes))))
	(tr (td (table (tr (td ,armed-checkbox) (td "My spaceship is armed.")))))
	(tr (td (table (tr (td ,shield-checkbox) (td "My spaceship has shields.")))))
	(tr (@ (valign "top"))
	    (td "My spaceship's drive is ")
	    (td (table ,@(table-radios drive-radios drives))))
	(tr))
       ,(make-submit-button "Submit choices"))
     ,(return-links main-return-link))))

;;; Size page HTML.
(define (generate-size-page new-url update-text class size-input)
  `(html 
    ,(make-title)
    (body 
     (h2 "Step 2 -- Specify crew size")
     (p "Please specify how many crew members your ship of class " ,class " 
will have. The builder will add as many treatment beds and accomodations as necessary to 
fullfill UFP Spaceship Crew's Rights Act 023/1000285.0/AB")
     ,(print-update update-text)
     (surflet-form ,new-url
       GET
       (table
	(tr (td "My ship is for a crew of ")
	    (td ,size-input)
	    (td "people"))
	(tr (td ,(make-submit-button)))))
     ,(return-links first-page-return-link main-return-link))))

;;; Text displayed if crew size is too big for the spaceship's class.
(define (complain-size class size)
  (format #f "Spaceships of the ~a class can only have 
up to ~a crew members. Please adjust the selected size or choose another 
spaceship class" class size))

;;; Text displayed if size is not positive.
(define positive-size
  "According to UFP Spaceship Crew Consistence Act 
025/100030.2/BX there must be at least one person on each spaceship. Thus, please 
specify a positive number.")

;;; HTML page generator for Step 3 and 4: Arming and shields.
;;; Shows the possible arming for selection and asks about the amount
;;; of energy for arming and shields.
(define (generate-armed+shield-page new-url update-text 
				    checkboxes+text energy-input
				    shield-input)
  `(html 
    ,(make-title)
    (body
     (surflet-form ,new-url
       GET
       ,(print-update update-text)
       ,(if (and checkboxes+text energy-input)
	    `((h2 "Step 3 -- Specify arming")
	      (p "Please select one or more arm types for your ship and amount of energy you want to spent for it or them, respectively.")
	      (p
	       (table ,@(map (lambda (checkbox+text)
			       `(tr (td ,(car checkbox+text))
				    (td ,(cdr checkbox+text))))
			     checkboxes+text)))
	      (p
	       (table (tr (td "Use") (td ,energy-input) (td "TW for weapons.")))))
	    '(h2 "Step 3 -- Done: No Arming"))
       ,(if shield-input
	    `((h2 "Step 4 -- Specify shields")
	      (p "Please specify the amount of energy you want to spent for your shields:")
	      (table (tr (td ,shield-input) (td "TJ"))))
	    '(h2 "Step 4 -- Done: No shields"))
       ,(make-submit-button "OK"))
     ,(return-links first-page-return-link main-return-link))))

;;; Text displayed, if arms' energy is not positive.
(define positive-energy 
  "Please specify a positive number for the amount of arms energy.")

;;; Text displayed if shield's energy is not positive.
(define positive-shield 
  "Please specify a positive number for the amount of shield energy.")

;;; Text displayed if no weapons are selected, though the customer
;;; wished to have an armed spaceship.
(define need-weapons
  "Please specify at least one weapon or turn back to main selection page and deselect arming.")

;;; Text displayed if arms' energy is too high for the spaceship class.
(define (arms-boundary class max-energy)
  (format #f "Spaceships of class ~a cannot spend more than ~a TW for their arming."
	  class max-energy))

;;; Text displayed if shield's energy is too high for the spaceship class.
(define (shield-boundary class max-shield)
  (format #f "Spaceships of class ~a cannot spend more than ~a TJ for their shields."
	  class max-shield))

;;; HTML page generator for the summary (order) page.
;;; Shows alle the details chosen for construction.
(define (generate-order-page new-url class weapons arms-energy
			     shield-energy drive size med-beds extras)
  `(html ,(make-title)
	 (body 
	  (h2 "Ordering")
	  (p "This are the data of your spaceship:")
	  (ul
	   (li "Class \"" ,class "\"")
	   (li ,size " crew members")
	   (li ,med-beds " treatment beds")
	   (li ,(if weapons
		    (format #f "Armed with ~a, powered with ~a TW"
			    (text-enumerate weapons) arms-energy)
		    "No arms"))
	   (li ,(if shield-energy 
		    (format #f "~a TJ of shield energy" shield-energy)
		    "No shields"))
	   (li ,drive " drive")
	   ,@(map (lambda (extra-text)
		    `(li ,extra-text))
		  extras))
	  (surflet-form ,new-url
	    POST
	    ,(make-submit-button "Order now"))
	  ,(return-links first-page-return-link main-return-link))))

;;; HTML page generator for the extras page.
;;; Shows a list of possible extras of this spaceship class for selection.
(define (generate-extras-page new-url class checkboxes+text)
  `(html 
    ,(make-title)
    (body
     (h2 "Step 5 -- Extras")
     (p "Select one or more extras that are available for
spaceships of class " ,class ":")
     (surflet-form ,new-url
       GET
       (table ,@(map (lambda (checkbox+text)
		       `(tr (td ,(car checkbox+text))
			    (td ,(cdr checkbox+text))))
		     checkboxes+text))
       ,(make-submit-button "OK"))
     ,(return-links first-page-return-link main-return-link))))
			      
;;; HTML page generator. 
;;; Shows the final page with a "Thank you" and an estimate for the
;;; construction time. Furthermore, it shows the customers host-name
;;; or its IP-adress.
(define (generate-finish-page months req)
  `(html ,(make-title)
	 (body (h2 "Ordered")
	       (p "Thank you for your ordering.")
	       (p "Your order has been registered. "
		  "We will contact you (" 
		  ,(host-name-or-ip (socket-remote-address 
				     (surflet-request-socket req)))
		  ") as soon as the ship is built.")
	       (p "This will take about " ,months " months.")
	       ,(return-links first-page-return-link main-return-link))))

(define main-return-link
  '(url "/" "Return to main menu."))

(define (previous-page-return-link prev)
  `(url ,prev "Return to previous page."))

(define first-page-return-link
  '(url "/surflet/spaceship.scm" "Return to spaceship builder entry page."))

(define (return-links . links)
  `(p
    (hr)
    ,@(map (lambda (link) (list link '(br)))
	   links)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helping functions

;;; Creates HTML-table rows, putting a radio in front of a text.
(define (table-radios radios texts)
  (map (lambda (radio text)
	 `(tr (td ,radio) (td ,text)))
       radios texts))

;;; Adds the 'checked attribute to a radio button, if its value
;;; (stored in LIST) equals to CHECK-THIS. With this, the selected
;;; value of a radio list can be restored if the page is redisplayed.
(define (checked-radio list check-this)
  (map (lambda (elem)
	 (if (equal? elem check-this)
	     (cons elem `(@ (checked)))
	     elem))
       list))

;;; Returns the ship-data structure for the class of name NAME.
(define (ship-ref name)
  (find (lambda (ship) (string=? (ship-data-class ship) name))
	ships))

;;; "Prints" UPDATE-TEXT in red color, i.e. in an HTML paragraph
;;; block.
(define (print-update update-text)
  `(p ,(and update-text `(font (@ (color "red")) ,update-text))))

;;; Same as R5RS member, except that it returns either #t or #f.
(define (member? thing list)
  (if (member thing list) #t #f))

;;; Makes an enumeration of the strings in TEXT-LIST:
;;; (text-enumerate '("John", "Bill", "Juliet")
;;; => "John, Bill and Juliet"
;;; with reasonable results if the list's length is smaller than 2.
(define (text-enumerate text-list)
  (let ((len (length text-list)))
    (case len
      ((0) "")
      ((1) (car text-list))
      ((2) (string-append (car text-list) " and " (cadr text-list)))
      (else
       (receive (head last)
	   (split-at text-list (- len 1))
	 (string-append (string-join head ", ")
			" and "
			(car last)))))))

;;; Does a check on the value of a number-input-field. Abstraction
;;; over two cases occured above. Best explained by the use above.
(define (check-bounded-number-field class input positiv selector boundary)
  (if (or (not input)
	  (<= input 0))
      positiv
      (let ((max (selector (ship-ref class))))
	(if (<= input max)
	    #f
	    (boundary class max)))))

  ))

