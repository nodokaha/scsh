;;; load.scm - a scheme web spidering script
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

(load "client.scm")

(define (spider-f html-dir)
  (spider-rec "" (list hostname) html-dir 0))

(define (spider-rec dir-file-name hostname-list html-dir index)
  (cond ((or (null? hostname-list)
             (string=? (car hostname-list) ""))
         (newline)("spidering ended.")(newline))
        (else
         (let ((dir-file-name-after (ask-server (string-append "GET / HTTP/1.0" (string #\return #\newline #\return #\newline)) "index.html" (car hostname-list) port)))
           (let ((file-contents (file->string (string-append dir-file-name-after "/" "index.html"))))
             (let ((url-list (file-contents->url file-contents 0)))
               (let ((hostname-list (append hostname-list (url->hostname url-list '())))
                     (keyword-list (file-contents->keyword file-contents keyword)))

                 (hash-set! table (string-append keyword (number->string index)) file-contents);;NOTE keys are variable due to append above
                 (spider-rec dir-file-name-after (cdr hostname-list) html-dir (+ index 1))
                 )))))))


(display "give hostname name : ")
(define hostname (symbol->string (read)))
(newline)
(display "give directory on host (type none for 'GET /' fetch) : ")
(define html-dir (symbol->string (read)))
(newline)
(display "server name = ")(display hostname)
(newline)
(display "give port : ")
(define port (number->string (read)))
(newline)

(display "give string to search for (no spaces): ")
(define keyword (symbol->string (read)))
(newline)

(define table (make-hash-table HASHTABLESIZE))

(spider-f html-dir)

