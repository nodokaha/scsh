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

;; FIXME double in string-util-alt.scm
(define (url->hostname url-list hostname-list)
  (let ((s "")
        (rets "")
        (j 0)
        (url (if (null? url-list)
                 #f
                 (car url-list)))
        )

    ;;(display "URL=")(display url)(newline)

    (if url
        (begin
          (set! url (string-append url (string #\/)));;following /
          (if (>= (string-length url) 8)
              (begin
                (do ((i 0 (+ i 1)))
                    ((cond ((>= i (string-length url))
                            (set! j (string-length url)))
                           ((or (string=? s "http://")(string=? s "ftp://")
                                (string=? s " http://")(string=? s " ftp://"))
                            (set! j i)))
                   #t)
                  (set! s (string-append s (string (string-ref url i))))
                )

                (do ((i j (+ i 1)))
                    ((cond ((>= i (string-length url))
                          #t)
                           ((not (eq? #\/ (string-ref url i)))
                            (set! j i)))
                     #t)
                  )

                (do ((i j (+ i 1)))
                    ((or (>= i (string-length url))
                         (eq? (string-ref url i) #\space)
                         (eq? (string-ref url i) #\newline)
                         (eq? (string-ref url i) #\/)
                         (eq? (string-ref url i) #\\))
                     #t)
                  (set! rets (string-append rets (string (string-ref url i))))
                  )

                (display rets)

                (set! hostname-list (append (list rets) (url->hostname (cdr url-list) hostname-list)))
                )
              hostname-list)
          hostname-list)
        (begin
          ;;(display s)
          hostname-list))
    hostname-list))

;;test

;;(display (url->hostname "http://soft/vub/"))

(define (file-contents->url-2 file-contents index)
  (let ((s "")
        (url-list '()))
    (do ((i (+ index 1) (+ i 1)))
        (;;(cond ((>= i (string-length file-contents))
         ;;       url-list)
         ;;      (
         (and (not (null? url-list))
              (>= i (string-length file-contents)))
         url-list)
      ;;))
      (cond
       ((and (string<=? "http://" s)
             (eq? (string-ref file-contents i) #\/))

        (set! url-list
              (append url-list (list s))))
        (set! s "")
        )

      (set! s (string-append s (string (string-ref file-contents i)))))
    ))

(define (file-contents->url file-contents index)
  (let ((s "")
        (url-list '())
        )
    (do ((i index (+ i 1)))
        ((or (>= i (string-length file-contents))
             (eof-object? (string-ref file-contents i)))
         s)
      (cond ((or (eq? (string-ref file-contents i) #\>)
                 ;;(eq? (string-ref file-contents i) #\space)
                 (eq? (string-ref file-contents i) #\newline))
             (set! s ""))
            ((and (< (+ i 8) (string-length file-contents))
                  (or
                   (and (eq? #\< (string-ref file-contents i))
                        (eq? #\space (string-ref file-contents (+ i 1)))
                        (eq? #\h (string-ref file-contents (+ i 2)))
                        (eq? #\r (string-ref file-contents (+ i 3)))
                        (eq? #\e (string-ref file-contents (+ i 4)))
                        (eq? #f (string-ref file-contents (+ i 5)))
                        (eq? #\= (string-ref file-contents (+ i 6)))
                        (eq? #\" (string-ref file-contents (+ i 7))))
                   (and (eq? #\< (string-ref file-contents i))
                        (eq? #\space (string-ref file-contents (+ i 1)))
                        (eq? #\H (string-ref file-contents (+ i 2)))
                        (eq? #\R (string-ref file-contents (+ i 3)))
                        (eq? #\E (string-ref file-contents (+ i 4)))
                        (eq? #\F (string-ref file-contents (+ i 5)))
                        (eq? #\= (string-ref file-contents (+ i 6)))
                        (eq? #\" (string-ref file-contents (+ i 7))))))
             (display "found valid <A href")(newline))
            ((or (string=? s "<A HREF=") ;;FIXME string>=? x 4
                 (string=? s "><A HREF=")
                 (string=? s " ><A HREF=")
                 (string=? s " <A HREF="))
             (display "found valid href")(newline)
             (display "OK")(display i)
             (set! url-list (append url-list (list (file-contents->url-2 file-contents i))))
             url-list))
      (set! s (string-append s (string (string-ref file-contents i))))
      )
    url-list))


;;test

;; (let ((in (open-file "index.html" "r")))
;;   (let ((file-contents ""))
;;     (do ((c (read-char in)(read-char in)))
;;         ((eof-object? c)
;;          #t)
;;       (set! file-contents (string-append file-contents (string c))))

;;     (let ((url-list (file-contents->url file-contents 0))
;;           (hostname-list '()))
;;       (url->hostname (car url-list) hostname-list);;FIXME url-list
;;       (display hostname-list)
;;       )))
