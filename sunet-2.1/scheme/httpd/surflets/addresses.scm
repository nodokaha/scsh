;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; return address

;; generates an unique return-address
;; may be used like 
;; (let ((address (make-address)))
;;   (send-html/suspend 
;;     (lambda (new-url)
;;       ...
;;       (url (address new-url) "Click here to get more")...)                

(define-record-type address :address
  (make-address-record name annotated? annotations)
  (name address-name)
  (annotated? address-annotated?)
  (annotations address-annotations set-address-annotations!))

(define (really-make-address name annotated?)
  (if annotated?
      (make-address-record name annotated? #f)
      (make-address-record name annotated? '())))

(define (address-add-annotation! address annotation)
  (let ((index (generate-unique-name "val")))
    (set-address-annotations! address
			      (cons (cons index annotation)
				    (address-annotations address)))
    index))

(define (address-annotation address index)
  (cond 
   ((assoc index (address-annotations address)) => cdr)
   (else #f)))

(define (make-address)
  (let ((address (really-make-address 
		  (generate-unique-name "return") #f)))
    (lambda (message)
      (cond
       ((string? message)
	(string-append message "?" (address-name address) "="))
       ((eq? message 'address)
	address)
       (else
	(error "address: unknown message/bad argument" 
	       message (address-name address)))))))

(define (make-annotated-address)
  (let ((address (really-make-address 
		  (generate-unique-name "return")
		  #t)))
    (lambda (message . annotation)
      (cond
       ((and (string? message)
	     (<= (length annotation) 1))
	(let ((index (if (null? annotation)
			 (address-add-annotation! address "")
			 (address-add-annotation! address (car annotation)))))
	  (string-append message "?" (address-name address)
			 "=" index)))
       ((eq? message 'address)
	address)
       (else
	(error "annotated-address: unknown message/bad argument(s)" 
	       message (address-name address)))))))

      

