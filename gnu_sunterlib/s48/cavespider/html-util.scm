;;
;;;;; html.scm - html utilities
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

(load "string-util.scm")

(define (read-html-file-iter in f)
  (let ((contents ""))
    (let ((tagged 0))
      (do ((c (read-char in) (read-char in)))
          ((eof-object? c)contents)
        (cond ((and (= tagged 0)(eq? c #\<))
               (set! tagged (+ tagged 1)))
              ((and (> tagged 0)(eq? c #\<))
               (set! tagged (+ tagged 1)))
              ((and (= tagged 0)(eq? c #\>))
               (set! tagged (- tagged 1)))
              ((and (> tagged 0)(eq? c #\>))
               (set! tagged (- tagged 1)))
              ((< tagged 0)
               (display "html-dump : bad html.")(newline)
               (set! tagged 0))
              )
        (set! contents (string-append contents (f c tagged)))
        ))
    contents))



(define (html-tags htmlfile)
  (let ((in (open-input-file htmlfile))
       )

    (define (f c tagged)
      (if (> tagged 0) (string c) ""))

    (read-html-file-iter in f)))

(define (html-dump htmlfile)
  (let ((in (open-input-file htmlfile))
        )

    (define (f c tagged)
      (if (= tagged 0) (string c) ""))

    (read-html-file-iter in f)))







;; http ripper
;;(define (html->url url)
;;  (let ((s "")
;;        )
;;    (do ((i 0 (+ i 1)))
;;        ((or (string=? s "http://")(string=? s "ftp://")
;;             (string=? s " http://")(string=? s " ftp://"))
;;         (set! j i))
;;      (set! s (string-append s (string (string-ref url i))))
;;      )))

