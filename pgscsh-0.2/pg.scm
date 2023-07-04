;; pg.scm -- socket level interface to the PostgreSQL RDBMS
;;
;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;; Version: 0.2
;; Time-stamp: <1999-09-14 emarsden>
;; Keywords: database dbms postgres sql interface
;; Copyright: (C) 1999  Eric Marsden
;;   
;;     This library is free software; you may redistribute it and/or
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
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>.
;; The latest version of this package should be available from
;;
;;     <URL:http://www.chez.com/emarsden/downloads/>


;;; Overview =========================================================
;;
;; This module lets you access the PostgreSQL object-relational DBMS
;; from within the Scheme Shell. It implements the client part
;; of the socket-level frontend/backend protocol, rather than
;; providing a wrapper around the libpq library. The module is
;; capable of type coercions from a wide range of SQL types to the
;; equivalent Scheme type, and supports large objects (BLOBs).
;;
;; NB: The postgres backend must be started with the "-i" argument in
;; order to allow TCP/IP connections; they are disallowed with the
;; default setup.


;;; Entry points =======================================================
;;
;; (pg:connect dbname user [password host port]) -> connection
;;     Connect to the database DBNAME on HOST (defaults to localhost)
;;     at PORT (defaults to 5432) via TCP/IP and log in as USER. If
;;     the database requires a password, send PASSWORD as clear text.
;;     Set the output date type to 'ISO', and initialize our type
;;     parser tables.
;;
;; (pg:exec connection . sql) -> pgresult
;;     Concatenate the SQL strings and send to the backend. Retrieve
;;     all the information returned by the database and return it in
;;     an opaque record PGRESULT.
;;
;; (pg:result pgresult what . args) -> info
;;     Extract information from the PGRESULT. WHAT can be one of 
;;          * 'connection
;;          * 'status
;;          * 'attributes
;;          * 'tuples
;;          * 'tuple tupleNumber
;;          * 'oid
;;     `connection' allows you to retrieve the database connection.
;;     `status' is a string returned by the backend to indicate the
;;     status of the command; it is normally "SELECT" for a select
;;     command, "DELETE 1" if the deletion affected a single row, etc.
;;     `attributes' is a list of tuples providing metadata: the first
;;     component of each tuple is the attribute's name as a string,
;;     the second an integer representing its PostgreSQL type, and the
;;     third an integer representing the size of that type. `tuples'
;;     returns all the data retrieved from the database, as a list of
;;     lists, each list corresponding to one row of data returned by
;;     the backend. `tuple num' can be used to extract a specific
;;     tuple. `oid' allows you to retrieve the OID returned by the
;;     backend if the command was an insertion; the OID is a unique
;;     identifier for that row in the database (this is
;;     PostgreSQL-specific, please refer to the documentation for more
;;     details).
;;
;; (pg:disconnect connection) -> #f
;;     Close the database connection.
;;
;; (pg:databases connection) -> list of strings
;;     Return a list of the databases available at this site (a
;;     database is a set of tables; in a virgin PostgreSQL
;;     installation there is a single database named "template1").
;;
;; (pg:tables connection) -> list of strings
;;     Return a list of the tables present in the database to which we
;;     are currently connected. Only include user tables: system
;;     tables are excluded.
;;
;; (pg:columns connection table) -> list of strings
;;     Return a list of the columns (or attributes) in TABLE, which
;;     must be a table in the database to which we are currently
;;     connected. We only include the column names; if you want more
;;     detailed information (attribute types, for example), it can be
;;     obtained from `pg:result' on a SELECT statement for that table.
;;
;;
;; (pg:lo-create conn . args) -> oid
;;     Create a new large object (BLOB, or binary large object in
;;     other DBMSes parlance) in the database to which we are
;;     connected via CONN. Returns an OID (which is represented as a
;;     Scheme integer) which will allow you to use the large object.
;;     Optional ARGS are a Unix-style mode string which determines the
;;     permissions of the newly created large object, one of "r" for
;;     read-only permission, "w" for write-only, "rw" for read+write.
;;     Default is "r".
;;
;; (pg:lo-open conn oid . args) -> fd
;;     Open a large object whose unique identifier is OID (a Scheme
;;     integer) in the database to which we are connected via CONN.
;;     Optional ARGS is a Unix-style mode string as for pg:lo-create;
;;     which defaults to "r" read-only permissions. Returns a file
;;     descriptor (a Scheme integer) which can be used in other
;;     large-object functions.
;;
;; (pg:lo-close conn fd)
;;     Close the file descriptor FD which was associated with a large
;;     object. Note that this does not delete the large object; use
;;     `pg:lo-unlink' for that.
;;
;; (pg:lo-read conn fd bytes) -> string
;;     Read BYTES from the file descriptor FD which is associated with
;;     a large object. Return a Scheme string which should be BYTES
;;     characters long.
;;
;; (pg:lo-write connection fd buf)
;;     Write the bytes contained in the Scheme string BUF to the
;;     large object associated with the file descriptor FD. 
;;
;; (pg:lo-lseek conn fd offset whence)
;;     Do the equivalent of a lseek(2) on the file descriptor FD which
;;     is associated with a large object; ie reposition the read/write
;;     file offset for that large object to OFFSET (an Scheme
;;     integer). WHENCE has the same significance as in lseek(); it
;;     should be one of SEEK_SET (set the offset to the absolute
;;     position), SEEK_CUR (set the offset relative to the current
;;     offset) or SEEK_END (set the offset relative to the end of the
;;     file). WHENCE should be a Scheme integer whose values can be
;;     obtained from the header file <unistd.h> (probably 0, 1 and 2
;;     respectively).
;;
;; (pg:lo-tell conn oid) -> integer
;;     Do the equivalent of an ftell(3) on the file associated with
;;     the large object whose unique identifier is OID. Returns the
;;     current position of the file offset for the object's associated
;;     file descriptor, as a Scheme integer.
;;
;; (pg:lo-unlink conn oid)
;;     Remove the large object whose unique identifier is OID from the
;;     system (in the current implementation of large objects in
;;     PostgreSQL, each large object is associated with an object in
;;     the filesystem).
;;
;; (pg:lo-import conn filename) -> oid
;;     Create a new large object and initialize it to the data
;;     contained in the file whose name is FILENAME. Returns an OID
;;     (as a Scheme integer). Note that is operation is only syntactic
;;     sugar around the basic large-object operations listed above.
;;
;; (pg:lo-export conn oid filename)
;;     Create a new file named FILENAME and fill it with the contents
;;     of the large object whose unique identifier is OID. This
;;     operation is also syntactic sugar.
;;
;;
;; Boolean variable `pg:disable-type-coercion' which can be set to #f
;; (before initiating a connection) to disable the library's type
;; coercion facility. Default is #t.


;;; Unimplemented =====================================================
;;
;; Only password authentication is supported (neither crypt nor
;; Kerberos work). The former would be easy to implement if it were
;; possible to access crypt() from scsh.
;;
;;
;; SECURITY NOTE ======================================================
;;
;; setting up PostgreSQL to accept TCP/IP connections has security
;; implications; please consult the documentation for details. pg.scm
;; supports neither the crypt authentication method, nor Kerberos.
;; However, it is possible to use the port forwarding capabilities of
;; ssh to establish a connection to the backend over TCP/IP. This
;; provides a secure authentication mechanism, and encryption (and
;; optionally compression) of data passing through the tunnel. Here's
;; how to do it (thanks to Gene Selkov, Jr. <selkovjr@mcs.anl.gov> for
;; the description):
;;
;; 1. Establish a tunnel to the backend machine, like this:
;; 
;; 	ssh -L 3333:wit.mcs.anl.gov:5432 postgres@wit.mcs.anl.gov
;; 
;;    The first number in the -L argument, 3333, is the port number of
;;    your end of the tunnel. The second number, 5432, is the remote
;;    end of the tunnel -- the port number your backend is using. The
;;    name or the address in between the port numbers belongs to the
;;    server machine, as does the last argument to ssh that also includes
;;    the optional user name. Without the user name, ssh will try the
;;    name you are currently logged on as on the client machine. You can
;;    use any user name the server machine will accept, not necessarily
;;    those related to postgres.
;; 
;; 2. Now that you have a running ssh session, you can point pg.scm at
;;    localhost at the port number you specified in step 1:
;;
;;         (pg:connect dbname user password "localhost" 3333)
;; 
;;    You can omit the port argument if you choose 5432 as your end of
;;    the tunnel, since pg.scm defaults to that.



;;; Portability =====================================================
;;
;; * scsh socket code
;; * scsh records
;; * let-optionals
;; * char->ascii and ascii->char


;;; TODO ============================================================
;;
;; * crypt authentication
;;
;; * add a mechanism for parsing user-defined types. The user should
;;   be able to define a parse function and a type-name; we query
;;   pg_type to get the type's OID and add the information to
;;   pg:*parsers*.
;;
;; * A higher-level interface designed to be portable across database
;;   systems.
;;
;; * Explicit support for transactions. For example, have an
;;   auto-commit mode which wraps each pg:exec in a transaction, with
;;   rollback on error, and a manual mode.
;;
;; * An s-expression encoding of SQL queries (like the sql-odbc
;;   package for Common Lisp)
;;
;; * DBI-like encoding of access parameters?
;;
;; * (pg:select connection query once-per-row-proc)
;;
;; * better exception handling



(define-record pgcon
  inport
  outport
  binary?)

(define-record pgresult
  connection
  status
  attributes
  tuples)

(define-syntax push
  (syntax-rules ()
     ((push new stack)
      (set! stack (cons new stack)))))

(define pg:NAMEDATALEN 32)              ; postgres_ext.h
(define pg:PG_PROTOCOL_LATEST_MAJOR 1)  ; libpq/pqcomm.h
(define pg:PG_PROTOCOL_LATEST_MINOR 0)
(define pg:SM_DATABASE 64)
(define pg:SM_USER     32)
(define pg:SM_OPTIONS  64)
(define pg:SM_UNUSED   64)
(define pg:SM_TTY      64)

(define pg:AUTH_REQ_OK       0)         ; libpq/pqcomm.h
(define pg:AUTH_REQ_KRB4     1)
(define pg:AUTH_REQ_KRB5     2)
(define pg:AUTH_REQ_PASSWORD 3)
(define pg:AUTH_REQ_CRYPT    4)

(define pg:STARTUP_MSG            7)
(define pg:STARTUP_KRB4_MSG      10)
(define pg:STARTUP_KRB5_MSG      11)
(define pg:STARTUP_PASSWORD_MSG  14)

(define pg:StartupPacketSize
  (+ 4 4 pg:SM_DATABASE pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY))

(define pg:MAX_MESSAGE_LEN    8192)     ; libpq-fe.h

(define pg:INV_ARCHIVE #x00010000)      ; fe-lobj.c
(define pg:INV_WRITE   #x00020000)
(define pg:INV_READ    #x00040000)
(define pg:LO_BUFIZE   1024)

(define pg:NULL '())
(define pg:null? null?)


;; FIXME should store host,port,password in the connection structure, to
;; be able to reconnect if necessary
(define (pg:connect dbname user . args)
  (let-optionals* args
       ((password "")
        (host "localhost")
        (port 5432)
        (sock (socket-connect protocol-family/internet socket-type/stream
                              host port))
        (inport (socket:inport sock))
        (outport (socket:outport sock))
        (connection (make-pgcon inport outport #f))
        (user-packet-length (+ pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY)))
    ;; send the startup packet
    (pg:send-int connection pg:StartupPacketSize 4)
    (pg:send-int connection pg:PG_PROTOCOL_LATEST_MAJOR 2)
    (pg:send-int connection pg:PG_PROTOCOL_LATEST_MINOR 2)
    (pg:send connection dbname pg:SM_DATABASE)
    (pg:send connection user user-packet-length)
    (pg:flush connection)
    (call-with-current-continuation
     (lambda (return)
       (let loop ()
         (case (read-char inport)
           ((#\E) (error "Backend error" (read-string 4096 inport)))
           ((#\R)
            (let ((areq (pg:read-net-int connection 4)))
              (cond
                ((= areq pg:AUTH_REQ_OK)
                 (and (not pg:disable-type-coercion)
                      (null? pg:*parsers*)
                      (pg:initialize-parsers connection))
                 (pg:exec connection "SET datestyle = 'ISO'")
                 (return connection))
                ((= areq pg:AUTH_REQ_PASSWORD)
                 (pg:send-int connection (+ 5 (string-length password)) 4)
                 (pg:send connection password)
                 (pg:send-int connection 0 1)
                 (pg:flush connection))
                ((= areq pg:AUTH_REQ_CRYPT)
                 ;; FIXME find a way to access crypt()
                 (error "Crypt authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB4)
                 (error "Kerberos4 authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB5)
                 (error "Kerberos5 authentication not supported"))
                (else
                 (error "Can't do that type of authentication" areq)))))
           (else
            (error "Problem connecting: expected an authentication response")))
         (loop))))))

(define (pg:exec connection . args)
  (call-with-current-continuation
   (lambda (return)
     (let ((inport (pgcon:inport connection))
           (outport (pgcon:outport connection))
           (sql (apply string-append args))
           (attributes '())
           (tuples '()))
       (if (> (string-length sql) pg:MAX_MESSAGE_LEN)
           (error "SQL statement too long" sql))
       (format outport "~a~a~a" #\Q sql (ascii->char 0))
       (pg:flush connection)
       (do ((c (read-char inport) (read-char inport)))
           ((eof-object? c) (error "EOF from backend"))
         ;; (format #t "In pg:exec got character ~a~%" c)
         (case c
           ((#\A)                       ; asynchronous notify
            (let ((pid (pg:read-int connection 4))
                  (msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
              (format #t "Asynchronous notify ~a" msg)))

           ((#\B)                       ; binary data transfer
            (set-pgcon:binary? connection #t)
            (if (null? attributes)
                (error "Tuple received before metadata"))
            (push (pg:read-tuple connection attributes) tuples))

           ((#\C)                       ; command status
            (let* ((status (pg:read-string connection pg:MAX_MESSAGE_LEN))
                   (result (make-pgresult connection status attributes
                                          (reverse tuples))))
              (return result)))
           
           ((#\D)                       ; text data transfer
            (set-pgcon:binary? connection #f)
            (if (null? attributes)
                (error "Tuple received before metadata"))
            (push (pg:read-tuple connection attributes) tuples))

           ((#\E)                       ; error message
            (let ((msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
              (error "Backend error" msg)))

           ;; indicates the end of a series of command statuses, for example
           ((#\I)                       ; empty query
            (let ((c (pg:read-char connection)))
              (if (char<? (ascii->char 0) c)
                  (error "Garbled data"))))

           ((#\N)                       ; error notification
            (let ((notice (pg:read-string connection pg:MAX_MESSAGE_LEN)))
              (format (error-output-port) "NOTICE: N~s~%" notice)))

           ((#\P)                       ; synchronous portal
            (let ((str (pg:read-string connection pg:MAX_MESSAGE_LEN)))
              ;; (format #t "Portal name ~a~%" str)
              #f))

           ((#\T)                       ; metadata field description
            (if (not (null? attributes))
                (error "Cannot handle multiple result group"))
            (set! attributes (pg:read-attributes connection)))
        
           (else
            (error "Unknown response type from backend" c))))))))
  
;; WHAT can be one of
;;    * 'connection
;;    * 'status
;;    * 'attributes
;;    * 'tuples
;;    * 'tuple tupleNumber
;;    * 'oid
(define (pg:result result what . arg)
  (cond ((eq? 'connection what) (pgresult:connection result))
        ((eq? 'status what)     (pgresult:status result))
        ((eq? 'attributes what) (pgresult:attributes result))
        ((eq? 'tuples what)     (pgresult:tuples result))
        ((eq? 'tuple what)
         (let ((which (if (pair? arg) (car arg) (error "which tuple?" arg)))
               (tuples (pgresult:tuples result)))
           (list-ref tuples which)))
        ((eq? 'oid what)
         (let ((status (pgresult:status result)))
           (if (string-ci=? "INSERT" (substring status 0 6))
               (string->number (substring status 7 (index status #\space 7)))
               (error "Only INSERT commands generate an oid" status))))
        (else
         (error "unknown result request" what))))

(define (pg:select connection query once-per-row-proc)
  #f)

(define (pg:disconnect connection)
  (let ((inport (pgcon:inport connection))
        (outport (pgcon:outport connection)))
    (write-char #\X outport)
    (force-output outport)
    (close inport)
    (close outport)
    #t))


;; Attribute information provided by the backend is as follows:
;;   attribute-name (string)
;;   attribute-type as an oid from table pg_type
;;   attribute-size (in bytes?)
(define (pg:read-attributes connection)
  (let ((attribute-count (pg:read-net-int connection 2))
        (inport (pgcon:inport connection))
        (attributes '()))
    (do ((i attribute-count (- i 1)))
        ((zero? i) (reverse attributes))
      (let ((type-name (pg:read-string connection pg:MAX_MESSAGE_LEN))
            (type-id   (pg:read-net-int connection 4))
            (type-len  (pg:read-net-int connection 2)))
        ;; ?make attributes an opaque type
        (push (list type-name type-id type-len) attributes)))))

;; a bitmap is a string, which we interpret as a sequence of bytes
(define (bitmap-ref bitmap ref)
  (let* ((char-ref (quotient  ref 8))
         (bit-ref  (remainder ref 8))
         (int (char->ascii (string-ref bitmap char-ref))))
    (bitwise-and #b10000000 (arithmetic-shift int bit-ref))))
    
;; the server starts by sending a bitmap indicating which tuples are
;; NULL
(define (pg:read-tuple connection attributes)
  (let* ((num-attributes (length attributes))
         (num-bytes (ceiling (/ num-attributes 8)))
         (bitmap (pg:read-chars connection num-bytes))
         (correction (if (pgcon:binary? connection) 0 -4))
         (tuples '()))
    (force-output)
    (do ((i 0 (+ i 1))
         (type-ids (map cadr attributes) (cdr type-ids)))
        ((= i num-attributes) (reverse tuples))
      (cond ((zero? (bitmap-ref bitmap i))
             (push pg:NULL tuples))
            (else
             (let* ((len (+ (pg:read-net-int connection 4) correction))
                    (raw (pg:read-chars connection (max 0 len)))
                    (parsed (pg:parse raw (car type-ids))))
               (push parsed tuples)))))))



;; large object support ================================================
;;
;; Humphrey: Who is Large and to what does he object?
;;
;; Large objects are the PostgreSQL way of doing what most databases
;; call BLOBs (binary large objects). In addition to being able to
;; stream data to and from large objects, PostgreSQL's
;; object-relational capabilities allow the user to provide functions
;; which act on the objects.
;;
;; For example, the user can define a new type called "circle", and
;; define a C or Tcl function called `circumference' which will act on
;; circles. There is also an inheritance mechanism in PostgreSQL. 
;;
;;======================================================================
(define pg:lo-initialized #f)
(define pg:lo-functions '())

(define (pg:lo-init connection)
  (let* ((res (pg:exec connection
                       "SELECT proname, oid from pg_proc WHERE "
                       "proname = 'lo_open' OR "
                       "proname = 'lo_close' OR "
                       "proname = 'lo_creat' OR "
                       "proname = 'lo_unlink' OR "
                       "proname = 'lo_lseek' OR "
                       "proname = 'lo_tell' OR "
                       "proname = 'loread' OR "
                       "proname = 'lowrite'")))
    (set! pg:lo-functions '())
    (for-each
     (lambda (tuple)
       (push (cons (car tuple) (cadr tuple)) pg:lo-functions))
     (pg:result res 'tuples))
    (set! pg:lo-initialized #t)))

;; fn is either an integer, in which case it is the OID of an element
;; in the pg_proc table, and otherwise it is a string which we look up
;; in the alist pg:lo-functions to find the corresponding OID.
(define (pg:fn connection fn integer-result . args)
  (or pg:lo-initialized (pg:lo-init connection))
  (let ((fnid (cond ((integer? fn) fn)
                    ((not (string? fn))
                     (error "Expecting a string or an integer" fn))
                    ((assoc fn pg:lo-functions) =>
                     (lambda (pair) (cdr pair)))
                    (else
                     (error "Unknown builtin function" fn)))))
    (pg:send-char connection #\F)
    (pg:send-char connection (ascii->char 0))
    (pg:send-int connection fnid 4)
    (pg:send-int connection (length args) 4)
    (for-each (lambda (arg)
               (cond ((integer? arg)
                      (pg:send-int connection 4 4)
                      (pg:send-int connection arg 4))
                     ((string? arg)
                      (pg:send-int connection (string-length arg) 4)
                      (pg:send connection arg))
                     (else
                      (error "Unknown fastpath type" arg))))
             args)
    (pg:flush connection)
;; ;; we should receive #\V on success or #\E on error
;;     (let ((c (pg:read-char connection)))
;;       (case c
;;         ((#\E) (error (pg:read-string connection 4096)))
;;         (else
;;          (error "Unexpected character in pg:fn" c))))
    (let loop ((result '()))
      (let ((c (pg:read-char connection)))
        (case c
          ((#\E) (error (pg:read-string connection 4096)))
          ((#\G)                        ; function returned OK
           (let* ((len (pg:read-net-int connection 4))
                  (res (if integer-result
                           (pg:read-net-int connection len)
                           (pg:read-chars connection len))))
             (loop res)))
          ((#\N)
           (let ((notice (pg:read-string connection pg:MAX_MESSAGE_LEN)))
             (format (error-output-port) "NOTICE: ~a~%" notice))
           (force-output)
           (loop result))
          ((#\0)
           result)
          ((#\V)                        ; bogus!
           (loop #t))
          (else
           (error "Unexpected character in pg:fn" c)))))))

;; returns an OID
(define (pg:lo-create connection . args)
  (let-optionals* args
      ((modestr "r")
       (mode (cond ((string-ci=? "r" modestr) pg:INV_READ)
                   ((string-ci=? "w" modestr) pg:INV_WRITE)
                   ((string-ci=? "rw" modestr)
                    (bitwise-ior pg:INV_READ pg:INV_WRITE))
                   ((integer? modestr) modestr)
                   (else (error "Bad mode" modestr))))
       (oid (pg:fn connection "lo_creat" #t mode)))
    (cond ((not (integer? oid))
           (error "Didn't return an OID" oid))
          ((zero? oid)
           (error "Can't create large object"))
          (else oid))))

;; args = modestring (default "r", or "w" or "rw")
;; returns a file descriptor for use in later pg:lo-* procedures        
(define (pg:lo-open connection oid . args)
  (let-optionals* args
     ((modestr "r")
      (mode (cond ((string-ci=? "r" modestr) pg:INV_READ)
                  ((string-ci=? "w" modestr) pg:INV_WRITE)
                  ((string-ci=? "rw" modestr)
                   (bitwise-ior pg:INV_READ pg:INV_WRITE))
                  ((integer? modestr) modestr)
                  (else (error "Bad mode" modestr)))))
     (pg:fn connection "lo_open" #t oid mode)))

(define (pg:lo-close connection fd)
  (pg:fn connection "lo_close" #t fd))

(define (pg:lo-read connection fd bytes)
  (pg:fn connection "loread" #f fd bytes))

(define (pg:lo-write connection fd buf)
  (pg:fn connection "lowrite" #t fd buf))
  
(define (pg:lo-lseek connection fd offset whence)
  (pg:fn connection "lo_lseek" #t fd offset whence))

(define (pg:lo-tell connection oid)
  (pg:fn connection "lo_tell" #t oid))
  
(define (pg:lo-unlink connection oid)
  (pg:fn connection "lo_unlink" #t oid))

;; returns an OID
(define (pg:lo-import connection filename)
  (let* ((fdin (open-input-file filename))
         (oid (pg:lo-create connection "rw"))
         (fdout (pg:lo-open connection oid "w")))
    (do ((str (read-string 1024 fdin)
              (read-string 1024 fdin)))
        ((not str))
      (pg:lo-write connection fdout str))
    (close fdin)
    (pg:lo-close connection fdout)
    oid))         

(define (pg:lo-export connection oid filename)
  (let* ((fdout (open-output-file filename))
         (fdin (pg:lo-open connection oid "r")))
    (do ((str (pg:lo-read connection fdin 1024)
              (pg:lo-read connection fdin 1024)))
        ((or (not str)
             (zero? (string-length str))
             (eof-object? str)))
      (format #t "Read ~s in pg:lo-export~%" str)
      (write-string str fdout))
    (pg:lo-close connection fdin)
    (close fdout)))


;; ===================================================================
;;
;; DBMS metainformation: list of databases present in the database
;; management system, list of attributes per database etc.
;;
;; \d -> SELECT datname FROM pg_database

(define (pg:databases connection)
  (let ((res (pg:exec connection "SELECT datname FROM pg_database")))
    (apply append (pg:result res 'tuples))))

(define (pg:tables connection)
  (let ((res (pg:exec connection "SELECT relname FROM pg_class, pg_user WHERE "
                      "(relkind = 'r' OR relkind = 'i' OR relkind = 'S') AND "
                      "relname !~ '^pg_' AND usesysid = relowner ORDER BY relname")))
    (apply append (pg:result res 'tuples))))

(define (pg:columns connection table)
  (let* ((sql (format #f "SELECT * FROM ~a WHERE 0 = 1" table))
         (res (pg:exec connection sql)))
    (map car (pg:result res 'attributes))))

         

;; type coercion support ==============================================
;;
;; When returning data from a SELECT statement, PostgreSQL starts by
;; sending some metadata describing the attributes. This information
;; is read by pg:read-attributes, and consists of each attribute's
;; name (as a string), its size (in bytes), and its type (as an oid
;; which points to a row in the system table pg_type). Each row in
;; pg_type includes the type's name (as a string).
;;
;; We are able to parse a certain number of the PostgreSQL types (for
;; example, numeric data is converted to a numeric Scheme type, dates
;; are converted to scsh date records, booleans to Scheme booleans).
;; However, there isn't a fixed mapping from a type to its OID which
;; is guaranteed to be stable across database installations, so we
;; need to build a table mapping OIDs to parsers.
;;
;; This is done by the procedure pg:initialize-parsers, which is run
;; the first time a connection is initiated with the database from
;; this invocation of scsh, and which issues a SELECT statement to
;; extract the required information from pg_type. This initialization
;; imposes a slight overhead on the first request, which you can
;; disable by setting pg:disable-type-coercion to #t if it bothers
;; you.
;; ====================================================================

(define pg:disable-type-coercion #f)

;; see man pgbuiltin for details on PostgreSQL builtin types
(define (pg:number-parser str) (string->number str))
(define (pg:text-parser str) str)
(define (pg:bool-parser str)
  (cond ((string=? "t" str) #t)
        ((string=? "f" str) #f)
        (else (error "Badly formed boolean from backend" str))))

;;  format for abstime/timestamp etc with ISO output syntax is
;;;    "1999-01-02 00:00:00+01"
;; which we convert to a scsh date record
(define (pg:isodate-parser str)
  (let ((year    (string->number (substring str 0 4)))
        (month   (string->number (substring str 5 7)))
        (day     (string->number (substring str 8 10)))
        (hours   (string->number (substring str 11 13)))
        (minutes (string->number (substring str 14 16)))
        (seconds (string->number (substring str 17 19)))
        (tz      (string->number (substring str 19 22))))
    (make-date seconds minutes hours day (- month 1) (- year 1900) tz)))  

(define (pg:initialize-parsers connection)
  (let* ((pgtypes (pg:exec connection "SELECT typname,oid FROM pg_type"))
         (tuples (pg:result pgtypes 'tuples)))
    (set! pg:*parsers* '())
    (for-each
     (lambda (tuple)
       (let* ((typname (list-ref tuple 0))
              (oid (string->number (list-ref tuple 1)))
              (type (assoc typname pg:type-parsers)))
         (if (pair? type)
             (push (cons oid (cdr type)) pg:*parsers*))))
     tuples)))

;; STR is an attribute's value retrieved as a string from the backend.
;; OID is the associated oid returned with the metadata. If we find a
;; parser for this type, we apply it, otherwise just return the
;; unparsed value.
(define (pg:parse str oid)
  (let ((parser (assq oid pg:*parsers*)))
    (if (pair? parser)
        ((cdr parser) str)
        str)))

;; alist of (oid . parser) pairs. This is built dynamically at
;; initialization of the connection with the database (once generated,
;; the information is shared between connections).
(define pg:*parsers* '())

(define pg:type-parsers
  `(("bool"      . ,pg:bool-parser)
    ("char"      . ,pg:text-parser)
    ("char2"     . ,pg:text-parser)
    ("char4"     . ,pg:text-parser)
    ("char8"     . ,pg:text-parser)
    ("char16"    . ,pg:text-parser)
    ("text"      . ,pg:text-parser)
    ("varchar"   . ,pg:text-parser)
    ("int2"      . ,pg:number-parser)
    ("int28"     . ,pg:number-parser)
    ("int4"      . ,pg:number-parser)
    ("oid"       . ,pg:number-parser)
    ("float4"    . ,pg:number-parser)
    ("float8"    . ,pg:number-parser)
    ("money"     . ,pg:number-parser)
    ("abstime"   . ,pg:isodate-parser)
    ("date"      . ,pg:isodate-parser)
    ("timestamp" . ,pg:isodate-parser)
    ("datetime"  . ,pg:isodate-parser)
    ("time"      . ,pg:text-parser)     ; preparsed "15:32:45"
    ("reltime"   . ,pg:text-parser)     ; don't know how to parse these
    ("timespan"  . ,pg:text-parser)
    ("tinterval" . ,pg:text-parser)))



;; support routines ===================================================

;; read an integer in network byte order (hope i got this right!)
(define (pg:read-net-int connection bytes)
  (do ((i bytes (- i 1))
       (inport (pgcon:inport connection))
       (accum 0))
      ((zero? i) accum)
    (set! accum (+ (* 256 accum) (char->ascii (read-char inport))))))

(define (pg:read-int connection bytes)
  (do ((i bytes (- i 1))
       (inport (pgcon:inport connection))
       (multiplier 1 (* multiplier 256))
       (accum 0))
      ((zero? i) accum)
    (set! accum (+ accum (* multiplier (char->ascii (read-char inport)))))))

(define (pg:read-char connection)
  (read-char (pgcon:inport connection)))

(define (pg:read-chars connection howmany)
  (do ((i 0 (+ i 1))
       (str (make-string howmany #\.))
       (inport (pgcon:inport connection)))
      ((= i howmany) str)
      (string-set! str i (read-char inport))))

;; read a null-terminated string
(define (pg:read-string connection maxbytes)
  (let ((inport (pgcon:inport connection)))
    (let loop ((i 0)
               (chars '()))
      (let ((c (read-char inport)))
        (cond ((or (= i maxbytes)
                   (eof-object? c)
                   (char=? c (ascii->char 0)))
               (list->string (reverse chars)))
              (else (loop (+ i 1) (cons c chars))))))))

;; highest order bits first
(define (pg:send-int connection int bytes)
  (let ((str (make-string bytes (ascii->char 0)))
        (outport (pgcon:outport connection)))
    (do ((i (- bytes 1) (- i 1)))
        ((negative? i))
      (string-set! str i (ascii->char (remainder int 256)))
      (set! int (quotient int 256)))
    (write-string str outport)))

(define (pg:send-int-reverse connection int bytes)
  (let ((str (make-string bytes (ascii->char 0)))
        (outport (pgcon:outport connection)))
    (do ((i 0 (+ i 1)))
        ((= i bytes))
      (string-set! str i (ascii->char (remainder int 256)))
      (set! int (quotient int 256)))
    (do ((i 0 (+ i 1)))
        ((= i bytes))
      (format #t "~s " (char->ascii (string-ref str i))))
    (newline)
    (write-string str outport)))      

(define (pg:send-char connection char)
  (write-char char (pgcon:outport connection)))

(define (pg:send connection str . maybe-length)
  (cond ((pair? maybe-length)
         ;; pad STR to length bytes
         (let* ((length (car maybe-length))
                (padding-length (- length (string-length str)))
                (padding (make-string padding-length (ascii->char 0)))
                (padded (string-append str padding)))
           (write-string padded (pgcon:outport connection))))
        (else
         (write-string str (pgcon:outport connection)))))

(define (pg:flush connection)
  (force-output (pgcon:outport connection)))

;; double any ' characters. What else needs quoting?
(define (pg:quote connection obj)
  (cond ((number? obj) (number->string obj))
        ((char? obj) (string #\' obj #\'))
        ((eq? #t obj) "'t'")
        ((eq? #f obj) "'f'")
        ((string? obj) (string-append "'" (string-replace "'" "''" obj) "'"))
        (else (error "Don't know how to quote this" obj))))

;; replace all occurrences of OLD by NEW in the string STR
(define (string-replace old new str)
  ;; I'm lazy
  (regexp-substitute/global #f (regexp-quote old) str 'pre new 'post))


;; EOF
