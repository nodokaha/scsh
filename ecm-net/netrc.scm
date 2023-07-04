;;; netrc.scm -- parse authentication information contained in ~/.netrc
;;
;; $Id: netrc.scm,v 1.1 1998/04/29 07:13:27 ecm Exp $
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



;;; Overview =====================================================
;;
;; On Unix systems the ~/.netrc file (in the user's home directory)
;; may contain information allowing automatic login to remote hosts.
;; The format of the file is defined in the ftp(1) manual page.
;; Example lines are
;; 
;;    machine ondine.cict.fr login marsden password secret
;;    default login anonymous password user@site
;;
;; The ~/.netrc file should be protected by appropriate permissions,
;; and (like /usr/bin/ftp) this library will refuse to read the file if
;; it is badly protected.


;;; Entry points =======================================================
;;
;; (user-mail-address) -> string
;;    Calculate the user's email address, as per the Emacs function of
;;    the same name. Will take into account the environment variable
;;    REPLYTO, if set.
;;
;; (netrc:default-login) -> string | #f
;;    Return the default login specified by the ~/.netrc file, or #f.
;;
;; (netrc:default-password) -> string | #f
;;    Return the default password specified by the ~/.netrc file, or #f.
;;
;; (netrc:lookup machine) -> string x string x string
;;     Return the login,password,account information for MACHINE
;;     specified by the ~/.netrc file.


;;; Related work ========================================================
;;
;; * Graham Barr has written a similar library for Perl, called
;;   Netrc.pm
;;
;; * ange-ftp.el (transparent remote file access for Emacs) parses the
;;   user's ~/.netrc file


;;; Portability ==================================================
;;
;; getenv, scsh file primitives, regexp code, format
;; define-record


(define (user-mail-address)
  (or (getenv "REPLYTO")
      (string-append (user-login-name) "@" (system-fqdn))))

(define (netrc:default-login)    *netrc:default-login*)
(define (netrc:default-password) *netrc:default-password*)

;;: string -> string x string x string
(define (netrc:lookup machine)
  (let ((record
         (find-suchthat (lambda (rec)
                          (and (equal? (netrc:machine rec) machine)
                               (list (netrc:login rec)
                                     (netrc:password rec)
                                     (netrc:account rec))))
                        *netrc*)))
    (values (netrc:login record)
            (netrc:password record)
            (netrc:account record))))

(define (netrc:lookup-password machine)
  (receive (login password account)
           (netrc:lookup machine)
           password))

(define (netrc:lookup-login machine)
  (receive (login password account)
           (netrc:lookup machine)
           login))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nothing exported below

(define-record netrc
  machine
  login
  password
  account)

(define *netrc* '())
(define *netrc:default-login* "anonymous")
(define *netrc:default-password* (user-mail-address))
(define *netrc:file* (resolve-file-name "~/.netrc"))


(define (netrc:parse)
  (netrc:check-permissions)
  (set! *netrc* '())
  (let ((fd (open-input-file *netrc:file*)))
    (for-each-line netrc:parse-line fd)))

;; raise error if any permissions are set for group or others.
(define (netrc:check-permissions)
  (let ((perms (- (file-mode *netrc:file*) 32768)))
    (if (positive? (bitwise-and #b000111111 perms))
        (error "Not parsing ~/.netrc file; dangerous permissions"))))

(define (netrc:try-match target line)
  (let ((match (string-match target line)))
    (and match
         (match:substring match 1))))

(define (netrc:parse-default line)
  (let ((login (netrc:try-match "login[ \t]+([^ \t]+)" line))
        (password (netrc:try-match "password[ \t]+([^ \t]+)" line)))
    (if login
        (set! *netrc:default-login* login))
    (if password
        (set! *netrc:default-password* password))))

(define (netrc:parse-line line)
  (cond ((string-match "default" line)
         (netrc:parse-default line))
        (else
         (let ((machine  (netrc:try-match "machine[ \t]+([^ \t]+)" line))
               (login    (netrc:try-match "login[ \t]+([^ \t]+)" line))
               (password (netrc:try-match "password[ \t]+([^ \t]+)" line))
               (account  (netrc:try-match "account[ \t]+([^ \t]+)" line)))
           (if (or machine login password account)
               (netrc:add machine login password account))))))

(define (netrc:add machine login password account)
  (set! *netrc* (cons (make-netrc machine login password account) *netrc*)))

;; for testing
(define (netrc:dump)
  (format #t "~%--- Dumping ~~/.netrc contents ---")
  (for-each (lambda (rec)
              (format #t "~%   machine ~a login ~a password ~a account ~a"
                      (netrc:machine rec)
                      (netrc:login rec)
                      (netrc:password rec)
                      (netrc:account rec)))
            *netrc*)
  (format #t "~%--- End of ~~/.netrc contents ---~%"))

(define (for-each-line proc fd)
  (let ((line (read-line fd)))
    (and (not (eof-object? line))
         (proc line)
         (for-each-line proc fd))))

(define (find-suchthat pred l)
  (if (null? l) #f
      (or (pred (car l))
          (find-suchthat pred (cdr l)))))

(netrc:parse)

;; EOF
