;			Verifying i/o extensions
;
; See OS-interf.scm for description of most of the extensions
; (write-string, port-copy, OS:remove-at-exit).
; See sys_open.c for more details on extended file names
;
; IMPORT
; appropriate prelude: myenv.scm, myenv-bigloo.scm, myenv-scm.scm
;	depending on your system
; catch-error.scm
;
; $Id: vext-io.scm,v 3.2 2003/08/11 21:03:14 oleg Exp oleg $

(declare 
 (block)
 (standard-bindings)
 (fixnum)
)

(cerr nl nl "--> Checking OS:ftell and OS:fseek-abs ..." nl)
(let
  ((file (OS:tmpnam))
   (fsize 4097)
   (pos->char (lambda (i)
		(integer->char (+ (modulo i 10)
				 (char->integer #\0))))))

  (call-with-output-file file
    (lambda (port)
      (do ((i 0 (++ i))) ((>= i fsize))
	(assert (= i (OS:ftell port)))
	(write-char (pos->char i) port))))

  (call-with-input-file file
    (lambda (port)
      (assert (zero? (OS:ftell port)))
      (peek-char port)
      (assert (zero? (OS:ftell port)))
      (read-char port)
      (assert (= 1 (OS:ftell port)))
      (peek-char port)
      (assert (= 1 (OS:ftell port)))
      (assert (zero? (OS:fseek-abs port 0)))
      (assert (char=? #\0 (read-char port)))
      (let ((i 1025))
	(assert (= i (OS:fseek-abs port i)))
	(assert (= i (OS:ftell port)))
	(assert (char=? (pos->char i) (read-char port))))
      (let ((i (-- fsize)))
	(assert (= i (OS:fseek-abs port i)))
	(assert (= i (OS:ftell port)))
	(assert (char=? (pos->char i) (read-char port))))
      ))

  (OS:remove file)
)
(cerr nl "Done" nl)


; Benchmarks results on Adric:
; baseline to file: 484 ms cpu time (472 user, 13 system), 8 minor faults
; write-str: 316 ms cpu time (316 user, 0 system), no faults
; if no -O2 flag while compiling OS-spec.str is used, the time is 320 ms
; writing to a str port: 2ms real/cpu time, in both cases

(cerr nl nl "--> testing and timing of write-string..." nl)
(let 
    ((work-file "/tmp/workfile")
     (io-amount 204800) ; bytes
     (str-amount 1025)) ; bytes

  (define (writing-baseline port how-many)
    (let loop ((i 0))
      (and (< i how-many)
	   (begin
	     (write-char (integer->char (##fixnum.logand i 255)) port)
	     (loop (++ i))))))

  (define writing-str 
    (let ((buffer (##make-string 8192 #\space)))
      (lambda (port how-many)
	(let loop ((left how-many))
	  (if (positive? left)
	      (let ((fill-len (min left (##string-length buffer))))
		(let fill ((i 0))
		  (if (>= i fill-len)
		      (begin
			(write-substring buffer 0 i port)
			(loop (- left fill-len)))
		      (begin
			(string-set! buffer i
				     (integer->char (##fixnum.logand i 255)))
			(fill (++ i)))))))))))

  (define (verify-file port how-many)
    (let loop ((i 0))
      (and (< i how-many)
	   (let ((c (read-char port)))
	     (if (eof-object? c)
		 (error "Unexpected EOF at pos " i))
	     (if (not (char=? c
			      (integer->char (##fixnum.logand i 255))))
		 (error "Char mismatch: found " (char->integer c)
			" at pos " i))
	     (loop (++ i))))))

  (cerr nl "Writing baseline, to a file " work-file nl)
  (call-with-output-file work-file
    (lambda (port)
      (time (writing-baseline port io-amount))))
  (call-with-input-file work-file
    (lambda (port)
      (verify-file port io-amount)))

  (cerr nl "Writing by chunks, to a file " work-file nl)
  (call-with-output-file work-file
    (lambda (port)
      (time (writing-str port io-amount))))
  (call-with-input-file work-file
    (lambda (port)
      (verify-file port io-amount)))

  (cerr nl "Writing baseline, to a string of " str-amount " chars " nl)
  (call-with-input-string
   (call-with-output-string
    (lambda (port)
      (time (writing-baseline port str-amount))))
   (lambda (port)
     (verify-file port str-amount)))

  (cerr nl "Writing by chunks, to a string of " str-amount " chars " nl)
  (call-with-input-string
   (call-with-output-string
    (lambda (port)
      (time (writing-str port str-amount))))
   (lambda (port)
     (verify-file port str-amount)))

  (cerr nl "Done" nl)
)

; Benchmarks results on Adric:
; baseline file-to-file: 474 ms cpu time (459 user, 16 system), 8 minor faults
; (almost the same time as baseline string-to-file!).
; stream-to-stream: 5 ms cpu time (5 user, 0 system), 4 minor faults
; stream-to-stream, explicit amount: the same
; baseline file-to-string: 424 ms cpu time (416 user, 8 system), 1072 minorf
; port-copy stream-to-string: 307 ms cpu time (299 user, 8 system), 672 mf

(cerr nl nl "--> testing and timing port-copy..." nl)
(let 
    ((work-file-i "/tmp/workfile-i")
     (work-file-o "/tmp/workfile-o")
     (io-amount 204800) ; bytes
     (str-amount 1025)) ; bytes

  ; write the test pattern into a stream
  (define (init-stream port from to)
    (let loop ((i from))
      (and (< i to)
	   (begin
	     (write-char (integer->char (##fixnum.logand i 255)) port)
	     (loop (++ i))))))

  ; Verify that the stream contains the expected bit pattern
  (define (verify-patterns port from to)
    (let loop ((i from))
      (and (< i to)
	   (let ((c (read-char port))
		 (i-exp (##fixnum.logand i 255)))
	     (cond
	      ((eof-object? c)
	       (error "Unexpected EOF at pos " (- i from)))
	      ((= i-exp (char->integer c))
	       (loop (++ i)))
	      (else
	       (error "unexpected pattern " (char->integer c)
		      " at pos " (- i from)
		      "; expected " i-exp)))))))

  (define (baseline-port-copy in-port out-port)
    (do ((c (read-char in-port) (read-char in-port)))
	((eof-object? c) c)
      (write-char c out-port)))

  (cerr nl "Initializing a pattern stream " work-file-i 
	" of size " io-amount nl)
  (call-with-output-file work-file-i
    (lambda (port) (init-stream port 0 io-amount)))

  (cerr nl "Baseline copying of the whole " work-file-i " into " 
	work-file-o nl)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (assert (eof-object?
		   (time (baseline-port-copy in-port out-port))))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (cerr nl "Using port-copy to copy the whole " work-file-i " into " 
	work-file-o nl)
  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (assert (eof-object?
		   (time (port-copy in-port out-port))))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (cerr nl "Using port-copy to copy " io-amount " bytes of "
	work-file-i " into " work-file-o nl)
  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (assert (zero?
		   (time (port-copy in-port out-port io-amount))))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (cerr nl "Checking interactions between read-char/peek-char and port-copy"
	nl)
  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (peek-char in-port)
	  (assert (zero?
		   (port-copy in-port out-port io-amount)))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (write-char (read-char in-port) out-port)
	  (assert (eof-object?
		   (port-copy in-port out-port io-amount)))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (write-char (read-char in-port) out-port)
	  (assert (zero?
		   (port-copy in-port out-port (-- io-amount))))))))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 0 io-amount)))

  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (read-char in-port)
	  (read-char in-port)
	  (assert (zero?
		   (port-copy in-port out-port (quotient io-amount 2))))))))
  (assert (= (quotient io-amount 2) (OS:file-length work-file-o)))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 2 (+ 2 (quotient io-amount 2)))))

  (OS:remove work-file-o)
  (call-with-input-file work-file-i
    (lambda (in-port)
      (call-with-output-file work-file-o
	(lambda (out-port)
	  (read-char in-port)
	  (read-char in-port)
	  (peek-char in-port)
	  (assert (zero?
		   (port-copy in-port out-port 1)))))))
  (assert (= 1 (OS:file-length work-file-o)))
  (call-with-input-file work-file-o
    (lambda (port) (verify-patterns port 2 3)))
  (OS:remove work-file-o)

  (cerr nl "Baseline copying of the whole " work-file-i " into a string" 
	nl)
  (let ((str
	 (call-with-input-file work-file-i
	   (lambda (in-port)
	     (call-with-output-string
	      (lambda (out-port)
		(assert (eof-object?
			 (time (baseline-port-copy in-port out-port))))))))))
    (call-with-input-string str
      (lambda (port) (verify-patterns port 0 io-amount))))

  (cerr nl "Using port-copy to copy the whole " work-file-i " into a string" 
	nl)
  (let ((str
	 (call-with-input-file work-file-i
	   (lambda (in-port)
	     (call-with-output-string
	      (lambda (out-port)
		(assert (eof-object?
			 (time (port-copy in-port out-port))))))))))
    (call-with-input-string str
      (lambda (port) (verify-patterns port 0 io-amount))))

  (cerr nl "Using port-copy to copy " io-amount " bytes of "
	work-file-i " into a string" nl)
  (let ((str
	 (call-with-input-file work-file-i
	   (lambda (in-port)
	     (call-with-output-string
	      (lambda (out-port)
		(assert (zero?
			 (time (port-copy in-port out-port io-amount))))))))))
    (call-with-input-string str
      (lambda (port) (verify-patterns port 0 io-amount))))

  (let ((str
	 (call-with-input-file work-file-i
	   (lambda (in-port)
	     (call-with-output-string
	      (lambda (out-port)
		(read-char in-port)
		(read-char in-port)
		(peek-char in-port)
		(assert (zero?
			 (port-copy in-port out-port str-amount)))))))))
    (assert (= str-amount (string-length str)))
    (call-with-input-string str
      (lambda (port) (verify-patterns port 2 (+ 2 str-amount)))))

  (cerr nl "Done" nl)
)

(cerr nl nl "--> Checking for zombie control..." nl)
(let*
  ((mypid (OS:getpid))
   (check-kids
     (lambda ()
       (OS:system "ps lw | grep " (number->string mypid)))))

  (cerr "My pid is " mypid nl)

  (assert (failed? (OS:waitpid 1))) ; no such child
  
  (let ((out-port
	  (open-output-file "| cat > /dev/null")))
    (assert (eq? #f (OS:waitpid 0 'NOHANG))) ; still running
    (let ((in-port (open-input-file "exit 0 |")))
      (cerr "Must be one zombie, one still running" nl)
      (check-kids)
      (let ((res (OS:waitpid 0)))
	(assert (pair? res) (= 0 (cdr res)))) ; 0 return code
      (assert (eq? #f (OS:waitpid 0 'NOHANG))) ; one is still running
      (close-input-port in-port)
      )
    (assert (eq? #f (OS:waitpid (- (OS:getpgrp)) 
		      'NOHANG))) ; one is still running, in the same group
    (close-output-port out-port)
    (assert (failed? (OS:waitpid 0))) ; should be reaped 
    )

  (cerr "Another pipe open should kill old zombies..." nl)
  (let* ((inp1 (open-input-file "exit 0 |"))
	 (_ (OS:sleep 1))
	 (inp2 (open-input-file "exit 2 |")))
      (cerr "Must be only one zombie" nl)
      (check-kids)
    (let ((res (OS:waitpid -1)))	; non-zero status
      (assert (pair? res) (positive? (cdr res))))
    (close-input-port inp1)
    (close-input-port inp2))

  (cerr "Should be no zombies" nl)
  (check-kids)
  (assert (failed? (OS:waitpid 0)))

  (cerr "port close should kill zombies..." nl)
  (let ((inp1 (open-input-file "exit 0 |")))
    (close-input-port inp1))

  (cerr "Should be no zombies" nl)
  (check-kids)
  (assert (failed? (OS:waitpid 0)))
  )
(cerr nl "Done" nl)

(cerr nl nl "--> Reading and writing a file through pipes..." nl)
(let
  ((file-name (OS:tmpnam))
   (file-size 75))

  (define (write-file n)
    (do ((i 0 (++ i))) ((>= i n))
      (display (integer->char (+ i (char->integer #\space))))))


  (define (read-check-file n)
    (do ((i 0 (++ i)) (c (read-char) (read-char)))
        ((>= i n) (assert (eof-object? c)))
      (let
        ((expected-char
            (integer->char (+ i (char->integer #\space)))))
        (if (not (equal? c expected-char))
          (error "read-check-error: expected '" expected-char
                 "' read '" c "' pos " i)))))

  (cerr "\topening " file-name " for writing via cat" nl)
  (OS:remove file-name)
  (with-output-to-file (string-append "| cat > " file-name)
    (lambda () (write-file file-size)))
  (OS:sleep 1)
  (cerr "\treading what we've written directly" nl)
  (assert (= (OS:file-length file-name) file-size))
  (with-input-from-file file-name
    (lambda () (read-check-file file-size)))

  (cerr "\tremoving the file and writing it again" nl)
  (OS:remove file-name)
  (assert (not (OS:file-exists? file-name)))
  (with-output-to-file (string-append " cat > " file-name "  | ")
    (lambda () (write-file file-size)))
  (OS:sleep 1)
  (cerr "\treading what we've written via cat" nl)
  (assert (= (OS:file-length file-name) file-size))
  (with-input-from-file (string-append "| cat  " file-name)
    (lambda () (read-check-file file-size)))

  (cerr "\tand again..." nl)
  (with-input-from-file (string-append "   cat  " file-name "|  ")
    (lambda () (read-check-file file-size)))
 
  (cerr "\tand again..." nl)
  (with-input-from-file (string-append "   cat " file-name "|  cat  |")
    (lambda () (read-check-file file-size)))
)
(cerr nl "Done" nl)

(cerr "\n\n--> Communicating with a file-command via a bidirectional pipe...\n")
(let ((pattern "1234567\r\n\007\r\n\000\001ABC")
      (file-name "| cat | cat"))
    (cerr "\tEchoing: opening a file '" file-name "'\n")
    (let ((io-port (##open-input-output-file file-name)))
      (cerr "\t\tsending pattern: " pattern nl)
      (display pattern io-port)
      (flush-output io-port)
      (cerr "\t\treading it back...")
      (do ((i 0 (++ i))) ((>= i (string-length pattern)))
          (let ((c-read (read-char io-port)) (c-pat (string-ref pattern i)))
            (write-char c-read ##stderr)
            (if (not (char=? c-read c-pat))
              (error "Read mismatch, pos " i " read >" c-read 
                "< expected >" c-pat "<\n"))))))
(let ((orig-string "Foo foofs here...")
      (subst-string "bar barfs here...")
      (file-name "| while read i; do echo $i | sed 's/[Ff]oo/bar/g'; done "))
    (cerr "\n\tPerforming a string substitution via sed ..."
          "\n\t\tOpening a file '" file-name "'\n")
    (let ((io-port (##open-input-output-file file-name)))
      (cerr "\t\tsending pattern: " orig-string nl)
      (display orig-string io-port)
      (display #\newline io-port)
      (flush-output io-port)
      (cerr "\n\t\tdone sending...\n")
      (with-input-from-port io-port
        (lambda ()
          (let ((read-str (next-token '() '(#\newline *eof*))))
            (cerr "\n\t\tread: " read-str nl)
            (assert (equal? read-str subst-string))
            (assert-curr-char '(#\newline) ""))))
      (cerr "\n\t\tone more time...\n")
      (display orig-string io-port)
      (display #\newline io-port)
      (flush-output io-port)
      (with-input-from-port io-port
        (lambda ()
          (let ((read-str (next-token '() '(#\newline *eof*))))
            (cerr "\n\t\tread: " read-str nl)
            (assert (equal? read-str subst-string))
            (assert-curr-char '(#\newline) ""))))
      )
)
(cerr nl "Done" nl)


(cerr nl nl "--> Checking of removing files at exit..." nl)
(let*
  ((file-name1 (OS:tmpnam)) (file-name2 (OS:tmpnam)))
  (cerr "\tcreating " file-name1 " and " file-name2 "...\n")
  (with-output-to-file file-name1 (lambda () (display "123")))
  (with-output-to-file file-name2 (lambda () (display "")))
  (cerr "\tregistering their removal at exit...\n")
  (OS:remove-at-exit file-name1)
  (OS:remove-at-exit file-name2)
  (cerr "\tmaking sure files exist...\n")
  (assert (OS:file-exists? file-name1))
  (assert (OS:file-exists? file-name2))
  (cerr "\t!!!Please make sure " file-name1 " and " file-name2 
    " are deleted when the program finishes!!!\n")
)
(cerr nl "Done" nl)

(cerr nl nl "--> Reading and writing a file which is a tcp pipe..." nl)
(let ((hostname (with-output-to-string
        (lambda () (with-input-from-file "hostname |"
          (lambda ()
            (do ((c (read-char) (read-char)))
              ((or (eof-object? c) (char=? c #\newline)))
              (write-char c))))))))
(assert (not (string-null? hostname)))
(let ((target-file (string-append "tcp://" hostname ":13")))
  (cerr "\tReading from a datetime port: opening a file '" target-file
    "'\nthe result is: "
    (lambda (port)
      (with-input-from-file target-file
        (lambda () (port-copy (current-input-port) port))))))

(let ((target-file (string-append "tcp://" hostname ":7"))
      (pattern "1234567\r\n\007\r\n\000\001ABC"))
    (cerr "\tEchoing: opening a file '" target-file "'\n")
    (let ((io-port (##open-input-output-file target-file)))
      (cerr "\t\tsending pattern: " pattern nl)
      (display pattern io-port)
      (flush-output io-port)
      (cerr "\t\treading it back...")
      (do ((i 0 (++ i))) ((>= i (string-length pattern)))
          (let ((c-read (read-char io-port)) (c-pat (string-ref pattern i)))
            (write-char c-read ##stderr)
            (if (not (char=? c-read c-pat))
              (error "Read mismatch, pos " i " read >" c-read 
                "< expected >" c-pat "<\n"))))))
)
(cerr nl "Done" nl)

(cerr nl nl "All tests passed" nl)
