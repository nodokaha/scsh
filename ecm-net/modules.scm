;; modules.scm -- Module definitions for ecm-net code
;;
;; $Id: modules.scm,v 1.1 1998/04/29 07:19:30 ecm Exp $
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



(define-structure ecm-utilities
  (export system-fqdn
          safe-first
          safe-second
          write-crlf
          dump)

  (open scsh
        scheme)
  (files ecm-utilities))


;; netrc.scm is a module for parsing ~/.netrc files, to obtain login
;; and password information for different network hosts.
(define-structure netrc
  (export user-mail-address
          netrc:default-login
          netrc:default-password
          netrc:lookup
          netrc:lookup-password
          netrc:lookup-login
          netrc:parse)

  (open defrec-package
        scsh        
        error-package
        ecm-utilities
        scheme)
  (files netrc))


;; ftp.scm is a module for transfering files between networked
;; machines using the File Transfer Protocol
(define-structure ftp
  (export ftp:connect
          ftp:login
          ftp:type
          ftp:rename
          ftp:delete
          ftp:cd
          ftp:cdup
          ftp:pwd
          ftp:rmdir
          ftp:mkdir
          ftp:modification-time
          ftp:size
          ftp:abort
          ftp:quit
          ftp:ls
          ftp:dir          
          ftp:get
          ftp:put
          ftp:append
          ftp:quot)

  (open netrc
        scsh
        defrec-package
        receiving
        handle
        conditions
        signals
        error-package
        ecm-utilities
        scheme)
  (files ftp))

                
;; pop3.scm is a module for accessing email on a maildrop server,
;; using the POP3 protocol.
(define-structure pop3
  (export pop3:connect
          pop3:login
          pop3:stat
          pop3:get
          pop3:headers
          pop3:last
          pop3:delete
          pop3:reset
          pop3:quit)

  (open netrc
        scsh
        defrec-package
        handle
        conditions
        signals
        ecm-utilities
        scheme)
  (files pop3))


;; nettime.scm is a module for requesting the time on remote machines,
;; using the time or the daytime protocol
(define-structure nettime
  (export net:time
          net:daytime)

  (open scsh
        scheme)
  (files nettime))

;; EOF
