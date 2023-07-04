;; Does space measurements using spatial
;; Copyright Andreas Bernauer, 2002

;;; Entry points:
;;;
;;; (PROFILE-SPACE [file-name])
;;; 
;;; Save output of (SPACE) to a file. The file is either a (fresh)
;;; temporary file (created with CREATE-TEMP-FILE) or the one
;;; specified by FILE-NAME. In either cases the FILE-NAME is returned.
;;;
;;;
;;; (PROFILE-RESULT file-name nth)
;;; 
;;; Return the result of the NTH output in the file named FILE-NAME as
;;; as SPACE-INFO object (see below for SPACE-INFO).
;;;
;;;
;;; (PROFILE-RESULTS file-name)
;;;
;;; Perform a repeated PROFILE-RESULT on FILE-NAME, returning a list
;;; that contains all SPACE-INFO objects (see below for SPACE-INFO).
;;;
;;;
;;; (WRITE-GNUPLOT-DATA-FILE output-file selector space-info-list)
;;;
;;; Creates a file named OUTPUT-FILE out of SPACE-INFO-LIST. The
;;; created file can be used with GNUPLOT (external program, not part
;;; of SUnet, http://www.gnuplot.info/) to create plot images.
;;; SELECTOR is a procedure that gets a SPACE-INFO object and returns
;;; an integer number. See below for possible SELECTORS on SPACE-INFO
;;; objects.
;;;
;;;
;;; SPACE-INFO object
;;;
;;; A container for all the information (SPACE) returns. Try 
;;;
;;; > ,open spatial
;;; > (space)
;;;
;;; to see, what I mean. See the following record definition for
;;; selectors and mutators. The lists returned by the selectors may be
;;; inspected with PURE-COUNT, PURE-BYTES, IMPURE-COUNT, IMPURE-BYTES,
;;; TOTAL-COUNT, TOTAL-BYTES
;;;
;;;
;;; Possible strategy to profile space usage: Spread calls to
;;; PROFILE-SPACE throughout your program. After you've finished, call
;;; PROFILE-RESULTS and feed the resulting list into
;;; WRITE-GNUPLOT-DATA-FILE. Run GNUPLOT (external program) with this
;;; file to get a plot image.
;;;
;;;
;;;
;;; Example:
;;; (write-gnuplot-data-file "result.dat"
;;;                          (lambda (space-info)
;;;                           (total-bytes (space-info-total space-info)))
;;;                          (profile-results "concatenated-profile-files"))
;;;
;;; in GNUPLOT:
;;; set terminal png
;;; set output "result.png"
;;; plot "result.dat" title "My profile" with lines


(define *debug* #f)

;; SPACE-INFO
(define-record-type space-info :space-info
  (make-space-info pair symbol vector closure location cell channel port 
		   ratnum record continuation extended-number template 
		   weak-pointer shared-binding unused-d-header1 
		   unused-d-header2 string byte-vector double bignum total)
  space-info?
  (pair space-info-pair set-space-info-pair!)
  (symbol space-info-symbol set-space-info-symbol!)
  (vector space-info-vector set-space-info-vector!)
  (closure space-info-closure set-space-info-closure!)
  (location space-info-location set-space-info-location!)
  (cell space-info-cell set-space-info-cell!)
  (channel space-info-channel set-space-info-channel!)
  (port space-info-port set-space-info-port!)
  (ratnum space-info-ratnum set-space-info-ratnum!)
  (record space-info-record set-space-info-record!)
  (continuation space-info-continuation set-space-info-continuation!)
  (extended-number space-info-extended-number set-space-info-extended-number!)
  (template space-info-template set-space-info-template!)
  (weak-pointer space-info-weak-pointer set-space-info-weak-pointer!)
  (shared-binding space-info-shared-binding set-space-info-shared-binding!)
  (unused-d-header1 space-info-unused-d-header1 set-space-info-unused-d-header1!)
  (unused-d-header2 space-info-unused-d-header2 set-space-info-unused-d-header2!)
  (string space-info-string set-space-info-string!)
  (byte-vector space-info-byte-vector set-space-info-byte-vector!)
  (double space-info-double set-space-info-double!)
  (bignum space-info-bignum set-space-info-bignum!)
  (total space-info-total set-space-info-total!))

;; FIELD-INSPECTORS
(define pure-count first)
(define pure-bytes second)
(define impure-count third)
(define impure-bytes fourth)
(define total-count fifth)
(define total-bytes sixth)


(define *run-count* 0)
(define *temp-file-lock* (make-lock))

;; PROFILE-SPACE writes result of call to (SPACE) to a file
(define (profile-space . maybe-file-name)
  (obtain-lock *temp-file-lock*)
  (let ((file-name
	 (:optional maybe-file-name
		    (create-temp-file "/var/tmp/profile"))))
    (let ((out (open-output-file file-name open/append)))
      (release-lock *temp-file-lock*)
      (set! *run-count* (+ 1 *run-count*))
      (format out "~%Run #~a~%" *run-count*)
      (with-current-output-port* out space)
      (close out))
    file-name))

;; PROFILE-RESULT returns a SPACE-INFO object representing the output
;; of (SPACE) of the NTH run. If there is no such run, 'EOF is returned.
(define (profile-result file-name nth)
  (let* ((in (open-input-file file-name))
	 (result (get-space-info in nth)))
    (close in)
    result))

;; PROFILE-RESULTS returns a list of all SPACE-INFO objects that may
;; be represented in FILE-NAME.
(define (profile-results file-name)
  (let ((in (open-input-file file-name)))
    (let loop ((space-info (get-space-info in 'first))
	       (result '()))
      (if (eq? 'eof space-info)
	  (reverse result)
	  (loop (get-space-info in 'first)
		(cons space-info result))))))
      

;; WRITE-GNUPLOT-DATA-FILE converts a SPACE-INFO-LIST into a data file
;; for a simple 2D plot, readable by GNUPLOT (external program, not
;; part of SUnet, http://www.gnuplot.info/)
(define (write-gnuplot-data-file output-file selector space-info-list)
  (let ((out (open-output-file output-file (bitwise-ior open/create open/truncate))))
    (display "# generated by profile.scm\n" out)
    (let loop ((count 0)
	       (space-info-list space-info-list))
      (if (null? space-info-list)
	  (close out)
	  (begin
	    (display count out)
	    (display " " out)
	    (display (selector (car space-info-list)) out)
	    (newline out)
	    (loop (+ 1 count)
		  (cdr space-info-list)))))))

;; GET-SPACE-INFO returns the result of the NTH call to (SPACE) stored
;; in IN. If NTH is 'FIRST, the SPACE-INFO object representing the
;; next run is returned. If EOF is reached before a run is found, 'EOF
;; is returned.
(define (get-space-info in nth)
  (if (eof-object? (skip-runs in nth))
      'eof
      (begin
	(skip-headers in)
	(let ((assoc-list (read-data in)))
	  (make-space-info
	   (get-record "pair" assoc-list)
	   (get-record "symbol" assoc-list)
	   (get-record "vector" assoc-list)
	   (get-record "closure" assoc-list)
	   (get-record "location" assoc-list)
	   (get-record "cell" assoc-list)
	   (get-record "channel" assoc-list)
	   (get-record "port" assoc-list)
	   (get-record "ratnum" assoc-list)
	   (get-record "record" assoc-list)
	   (get-record "continuation" assoc-list)
	   (get-record "extended-number" assoc-list)
	   (get-record "template" assoc-list)
	   (get-record "weak-pointer" assoc-list)
	   (get-record "shared-binding" assoc-list)
	   (get-record "unused-d-header1" assoc-list)
	   (get-record "unused-d-header2" assoc-list)
	   (get-record "string" assoc-list)
	   (get-record "byte-vector" assoc-list)
	   (get-record "double" assoc-list)
	   (get-record "bignum" assoc-list)
	   (get-record "total" assoc-list))))))

;; GET-RECORD returns the data of the element in ASSOC-LIST indexed by KEY.
(define (get-record key assoc-list)
  (let ((record (assoc key assoc-list)))
    (if record 
	(cdr record)
	(error "wrong data format - field missing" 'pair))))

;; SKIP-HEADERS just skips two lines in IN (the header lines of a
;; (SPACE) outpout)
(define (skip-headers in)
  (read-line in)
  (read-line in))

;; RUN-REGEXP matches the run number in the output file of
;; PROFILE-SPACE
(define run-regexp (rx "Run #" (submatch (* digit))))

;; SKIP-RUNS goes forward in IN, until a run in IN with number NTH is
;; found. If NTH is 'FIRST, just the next reachable run is reached. If
;; EOF occurs while searching, 'EOF is returned.
(define (skip-runs in nth)
  (let loop ((line (read-line in)))
    (if (eof-object? line)
	line
	(let ((match (regexp-search run-regexp line)))
	  (if (and match
		   (or (eq? 'first nth)
		       (= (string->number (match:substring match 1))
			  nth)))
	      #t
	      (loop (read-line in)))))))

;; TRIM-STRING->NUMBER returns the number stored in S, perhaps padded
;; with spaces on its left: (TRIM-STRING->NUMBER " 123") ==> 123
(define (trim-string->number s)
  (string->number (string-trim s)))

;; READ-DATA parses the output of (SPACES). It expects the next line
;; in IN to be the first data line (not a header). It returns an
;; a-list (row name . data-list)
(define (read-data in)
  (let loop ((count 0)
	     (line (read-line in))
	     (assoc-list '()))
;    (format #t "line: ~a~%" line)
    (if (< count 22)
	(loop (+ 1 count)
	      (read-line in)
	      (cons 
	       (cons (string-trim (substring/shared line 0 16)) ; name
		     (map trim-string->number
			  (list (substring/shared line 16 23) ;pure-count
				(substring/shared line 23 30) ;pure-bytes
				(substring/shared line 30 37) ;impure-count
				(substring/shared line 37 44) ;impure-bytes
				(substring/shared line 44 51) ;total count
				(substring/shared line 51 59)))) ;total bytes
	       assoc-list))
	assoc-list)))

