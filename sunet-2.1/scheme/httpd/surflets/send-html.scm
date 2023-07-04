;;; Allows sending of HTML represented in Oleg-like SXML-list instead
;;; of pure string.
;;; Copyright 2002,2003, Andreas Bernauer


;; Send surflet-sxml lists as HTML
(define (send-html/suspend html-tree-maker)
  (send/suspend 
   (lambda (new-url)
     (make-usual-html-response
      (sxml->string (html-tree-maker new-url) 
		    surflet-sxml-rules)))))

(define (send-html/finish html-tree)
  (do-html-sending send/finish html-tree))

(define (send-html html-tree)
  (do-html-sending send html-tree))

;; Semd strings as HTML
(define (send-html-string/suspend html-string-maker)
  (send/suspend 
   (lambda (new-url)
     (make-usual-html-response
      (html-string-maker new-url)))))

(define (send-html-string/finish html-string)
  (do-html-string-sending send/finish html-string))

(define (send-html-string html-string)
  (do-html-string-sending send html-string))

;; Helping functions
(define (do-html-sending sender html-tree)
  (do-html-string-sending 
   sender 
   (sxml->string html-tree surflet-sxml-rules)))

(define (do-html-string-sending sender html-string)
  (sender (make-usual-html-response html-string)))

;; This is not for public, as we add the no-cache header that is
;; needed for SUrflets.
(define (make-usual-html-response html-string)
  (make-surflet-response
   (status-code ok)
   "text/html"
   '(("Cache-Control" . "no-cache"))
   html-string))

