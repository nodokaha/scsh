#!/usr/local/bin/scsh \
-lm modules.scm -dm -o test -e main -s
!#

;; demo.scm -- Demonstration & testing of scsh pg module
;;
;;     Copyright (C) 1999  Eric Marsden
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
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>


;; This file runs a limited number of tests on the database code. It
;; assumes you have a database user "postgres" either with no password
;; or with password "postgres"; you can change the username and
;; password to something more appropriate. It uses a special "test"
;; database (which you will have to create first, either using
;; `create_database' from the commandline, or from psql using `CREATE
;; DATABASE'), and should clean up after itself. Provides an example
;; of how to write an executable scsh script, and how to use the
;; module system.
;;
;; * is the postmaster running?
;; * was the postmaster started with the -i commandline option?


(define-structure test
  (export main)
  (open pg
        scsh
        scheme)
  (begin

    (define (test-general)
      (let* ((conn (pg:connect "test" "postgres" "postgres"))
             (r1 (pg:exec conn "CREATE TABLE pgscmtest (a int, b VARCHAR(4))"))
             (r2 (pg:exec conn "INSERT INTO pgscmtest VALUES (3, 'zae')"))
             (r3 (pg:exec conn "INSERT INTO pgscmtest VALUES (66, 'poiu')"))
             (r4 (pg:exec conn "SELECT * FROM pgscmtest"))
             (r5 (pg:exec conn "DROP TABLE pgscmtest")))
        (format #t "~%==============================================~%")
        (format #t "status of CREATE is ~s~%" (pg:result r1 'status))
        (format #t "status of INSERT is ~s~%" (pg:result r2 'status))
        (format #t "oid of INSERT is ~s~%"    (pg:result r2 'oid))
        (format #t "status of SELECT is ~s~%" (pg:result r4 'status))
        (format #t "attributes of SELECT are ~s~%" (pg:result r4 'attributes))
        (format #t "tuples of SELECT are ~s~%" (pg:result r4 'tuples))
        (format #t "second tuple of SELECT is ~s~%" (pg:result r4 'tuple 1))
        (format #t "status of DROP is ~s~%" (pg:result r5 'status))
        (format #t "==============================================~%")
        (pg:disconnect conn)))

    (define (test-metadata)
      (let* ((conn (pg:connect "test" "postgres" "postgres"))
             (dbs (pg:databases conn))
             (tables (pg:tables conn)))
        (format #t "List of databases: ~s~%" dbs)
        (format #t "List of tables in test database: ~s~%" tables)
        (format #t "List of columns in the table ~s: ~s~%"
                (car tables) (pg:columns conn (car tables)))
        (pg:disconnect conn)))

    (define (test-date)
      (let* ((conn (pg:connect "test" "postgres" "postgres")))
        (pg:exec conn "CREATE TABLE pgscshtest (a timestamp, b abstime, c time)")
        (pg:exec conn "INSERT INTO pgscshtest VALUES "
                      "(current_timestamp, 'now', 'now')")
        (let* ((res (pg:exec conn "SELECT * FROM pgscshtest"))
               (parsed (car (pg:result res 'tuples))))
          (format #t "Timestamp = ~s~%abstime = ~s~%time = ~s~%"
                  (date->string (car parsed))
                  (date->string (cadr parsed))
                  (caddr parsed)))
        (pg:exec conn "DROP TABLE pgscshtest")
        (pg:disconnect conn)))

    ;; test of large-object interface
    (define (test-lo-read)
      (let* ((conn (pg:connect "test" "postgres" "postgres"))
             (oid (pg:lo-create conn "rw"))
             (fd (pg:lo-open conn oid "rw")))
        (format #t "==================================================~%")
        (pg:lo-write conn fd "Hi there mate")
        (pg:lo-lseek conn fd 0 0)       ; SEEK_SET = 0
        (if (not (zero? (pg:lo-tell conn fd)))
            (format #t "lo-tell test failed!~%"))
        (format #t "Read ~s from lo~%" (pg:lo-read conn fd 7))
        (format #t "==================================================~%~%")
        (pg:lo-close conn fd)
        (pg:lo-unlink conn oid)
        (pg:disconnect conn)))

    (define (test-lo-import)
      (let* ((conn (pg:connect "test" "postgres" "postgres"))
             (oid (pg:lo-import conn "/etc/group")))
        (pg:lo-export conn oid "/tmp/group")
        (cond ((zero? (run (diff /tmp/group /etc/group)))
               (format #t "lo-import test succeeded~%")
               (delete-file "/tmp/group"))
              (t
               (format #t "lo-import test failed: check differences~%")
               (format #t "between /etc/group and /tmp/group~%")))
        (pg:lo-unlink conn oid)
        (pg:disconnect conn)))
    
    
    (define (main . args)
      (test-general)
      (test-metadata)
      (test-date)
      (test-lo-read)
      (test-lo-import))

    
    ))

;; EOF
