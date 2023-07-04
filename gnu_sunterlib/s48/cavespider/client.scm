;;; client.scm - connect-to-server utility
;;;
;;; Copyright (c) 2012 Johan Ceuppens
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "util.scm")

;;,open posix posix-files

(define server-data
  (let* ((port1 80)
         (port2 8080)
         (hostname ""))
    (list hostname port1)))

(define (ask-server0 request hostname port)
  (call-with-values
    (lambda ()
      (socket-client hostname port))
    (lambda (in out)
       (display request out)
       (close-output-port out)
       (let ((answer (read in)));;(make-string-input-port in)))
      (close-input-port in)
      answer))))


(define (get-addr hostname)
  (let ((host (gethostbyname hostname)))
    (car (hostent:addr-list host))))

(define (file->contents filename)
 (let ((in (open-input-file filename)))
   (let ((file-contents ""))
     (do ((c (read-char in)(read-char in)))
         ((eof-object? c)
          file-contents)
       ;;(display c)
       (set! file-contents (string-append file-contents (string c))))
;;     (file-contents->url file-contents))))
     )))


(define (ask-server request filename hostname port)
  (let* ((dir-filename (do ((i 0 (+ i 1)))
                           ((make-directory (string-append "./" hostname (number->string i)))
                            (string-append "./" hostname (number->string i))) ;; return val
                         ))
         (out-file-port (open-output-file (string-append dir-filename "/" filename)))
         )
    (call-with-values
      (lambda ()
      	(socket-client hostname port))
      (lambda (in out)
    (display request out)

    (do ((c (read-char in)(read-char in)))
        ((eof-object? c) #t)
      ;;(display c)
      (display c out-file-port)
      ;;(display c)
      )
    ))
    dir-filename))


;    (let ((contents (file->contents (string-append dir-filename "/" filename))))
;      (display contents)
;      (file-contents->url contents)
;      )))
;;test
;;(ask-server (string-append "GET / HTTP/1.0" (string #\return #\newline #\return #\newline) "index.html" "www.gnu.org") 80)
