#!/usr/local/bin/scsh \
-lm modules.scm -dm -o test -e run-tests -s
!#

;; $Id: test.scm,v 1.1 1998/04/29 07:18:48 ecm Exp $
;;
;;     Copyright (C) 1998  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
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



;; This file runs a limited number of tests on the networking code. It
;; provides an example of how to write an executable scsh script, and
;; how to use the module system.


(define-structure test
  (export run-tests)
  (open ftp
        nettime
        pop3
        scsh
        scheme)
  (begin

    (define (run-tests . args)
      (ftp:test)
      (pop3:test)
      (time:test))

    ;; Beware, passwords are written in clear text in the log file.
    ;; ftp://ftp.info.iut-tlse3.fr/pub/debian
    ;; ftp.cict.fr ls-lR.gz
    ;; ftp.cdrom.com:/README  ok
    ;; sunsite.unc.edu:/WELCOME
    ;; ftp.lip6.fr:/new-this-week
    (define (ftp:test)
      (let ((connection (ftp:connect "sunsite.unc.edu" "/tmp/ftpLog.txt"))
            (file "/WELCOME"))
        (newline)
        (display "*** ftp:test starting ***")
        (tester (ftp:login connection "anonymous"))
        (tester (ftp:type connection 'binary))
        (tester (ftp:pwd connection))
        (tester (ftp:cd connection "/pub"))
        (tester (ftp:cdup connection))
        (tester (ftp:dir connection))
        (tester (ftp:size connection file))
        ;; following require write perms
        ;; (tester (ftp:mkdir connection "/tmp/FTPDIR"))
        ;; (tester (ftp:rename connection "/tmp/FTPDIR" "/tmp/NEWFTPDIR"))
        ;; (tester (ftp:rmdir connection "/tmp/NEWFTPDIR"))
        ;; (tester (ftp:put connection "/tmp/README" "/tmp/newREADME"))
        (tester (date->string (ftp:modification-time connection file)))
        (tester (ftp:get connection file "/tmp/got"))
        (tester (ftp:quot connection "SITE HELP"))
        (tester (ftp:quit connection))
        (newline)
        (display "*** ftp:test done ***")
        (newline)
        ))


    ;; this test supposes you have at least one message waiting for
    ;; you in your maildrop. It should not change the state of your
    ;; maildrop. Beware, passwords are written in cleartext in the log
    ;; file.
    (define (pop3:test)
      (let ((connection (pop3:connect "localhost" "/tmp/popLog.txt")))
        (newline)
        (display "*** pop3:test starting ***")
        (tester (pop3:login connection "ecm" "secret"))
        (receive (count bytes)
                 (pop3:stat connection)
                 (format #t "~%(pop3:stat connection) => ~a x ~a~%" count bytes))
        (tester (pop3:get connection 1))
        (tester (pop3:headers connection 1))
        (tester (pop3:last connection))
        (tester (pop3:delete connection 1))
        (tester (pop3:reset connection))
        (tester (pop3:quit connection))
        (newline)
        (display "*** pop3:test done ***")
        (newline)
        ))


    (define (time:test)
      (newline)
      (display "*** time:test starting ***")
      (format #t "~%Remote time is ~a"
              (net:time "www.cict.fr" socket-type/stream))
      (format #t "~%Remote daytime is ~a"
              (net:daytime "127.0.0.1" socket-type/stream))
      (format #t "~%Local time is ~a" (time))
      (newline)
      (display "*** time:test done ***")
      (newline))

    
    (define-syntax tester
      (syntax-rules ()
                    ((tester x)
                     (format #t "~%~a => \"~a\"" 'x x))
                    ))

    ))

;; EOF
