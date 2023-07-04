;; ecm-utilities.scm -- Utility procedures for ecm-net code
;;
;; $Id: ecm-utilities.scm,v 1.1 1998/04/29 07:18:40 ecm Exp $
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


;; please tell me if this doesn't work on your system.
(define (system-fqdn)
  (let ((sysname (system-name)))
    (if (index sysname #\.)
        sysname
        (nslookup-fqdn))))

(define (nslookup-fqdn)
  (let* ((cmd (format #f "nslookup ~a" (system-name)))
         (raw (join-strings (run/strings (nslookup ,(system-name)))))
         (match (string-match "Name: +([-a-zA-Z0-9.]+)" raw)))
    (display raw)
    (match:substring match 1)))


;; prefer this to :optional
(define (safe-first x) (and (not (null? x)) (car x)))
(define (safe-second x) (and (not (null? (cdr x))) (cadr x)))

(define (write-crlf port)
  (write-string "\r\n" port)
  (force-output port))


(define (dump fd)
  (let loop ((c (read-char fd)))
    (cond ((not (eof-object? c))
           (write-char c)
           (loop (read-char fd))))))

  
(define-syntax when
  (syntax-rules ()
    ((when bool body1 body2 ...)
     (if bool (begin body1 body2 ...)))))


(define-syntax unless
  (syntax-rules ()
    ((unless bool body1 body2 ...)
     (if (not bool) (begin body1 body2 ...)))))

;; EOF
