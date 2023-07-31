;****************************************************************************
; 					Parse the URL parameters string
;
; -- procedure+: CGI:url-unquote QUERY-STRING
;
; Converts a CGI-quoted string like
; "parm1=val1&parm2=val2+val%253+val4&%7Eparm3=&parm4"
; into an assoc list
; '((parm4) (~parm3 "")
;   (parm2 "val2" "val%3" "val4")
;   (parm1 "val1"))
;
; Parsing is done by a finite state machine, that takes into
; account the current state (looking for a parm, looking for a value,
; looking for a continuation of parm/value after %xx quoted-char),
; action-prefix ('=', '+', or '&') and the token that follows
; the action prefix (that is, a set of characters through the next
; action-prefix or the end-of-string). At the very beginning, the
; action-prefix is assumed '&'.
;
; This function is intended to be used for parsing QUERY_STRING
; (or similar parameters) passed by a server to a CGI script.
;
; Tests
; (CGI:url-unquote "aaa")
; (CGI:url-unquote "aaa=")
; (CGI:url-unquote "aaa=&bbb=b1&ccc=c1+c2&ddd")
; (CGI:url-unquote "aaa=/bbb=b1/ccc=c1+c2/ddd")
; (CGI:url-unquote "%7eaaa=&%25b%25bb=b%201&c%20c%7E=c1+c2&ddd%21")
; Note, that if the string mentions some parameter twice, the
; _last_ mentioning takes precedence!
; Also note that empty string tokens like in QUERY_STRING
; "parm=+val++val+&parm2" are skipped (save the very last one)
; That means that leading spaces from parameter values are going to
; be skipped.
;
; $Id: CGI-unquote.scm,v 1.2 1997/11/17 21:46:19 oleg Exp oleg $

; (require "input-parse.scm")

(define (CGI:url-unquote parm-string)
  (let ((result '())
        (read-primitive-token
          (lambda ()
            (if (eof-object? (peek-char)) ""
              (next-token '() '(#\= #\+ #\& #\% *eof*) "URL-unquoting")))))
            
    (with-input-from-string parm-string
      (lambda ()
        (do ((action-prefix #\& (read-char)) (status 'init)
             (vals '()) (keyword #f))
           ((eq? status 'stop) result)
           
           (let
             ((token (read-primitive-token)))

             	; If #\% left on stream, read it and the following
             	; two characters (hex digits), unquote the char and
             	; append it to the rest of the token
             (do () ((not (eq? (peek-char) #\%)))
               (read-char)		; read the percent char
               (let ((quoted-char-str (make-string 2)))
                 (string-set! quoted-char-str 0 (read-char))
                 (string-set! quoted-char-str 1 (read-char))
                 (let ((quoted-char (string->number quoted-char-str 16)))
                   (set! token
                     (string-append token 
                       (if quoted-char (string (integer->char quoted-char))
                         "*INVALID-%-SEQ*")
                       (read-primitive-token))))))
             
             (if (eof-object? action-prefix)
               (set! action-prefix '*eof*))
             (set! status
               (case action-prefix 
                 ((#\& *eof*)		; new parmset to follow
                   (case status
                     ((init) #t)
                     ((have-read-keyword)	; parm without any values
                       (set! result (cons (list keyword) result)))
                     ((have-read-value)
                       (set! result (cons (cons keyword (reverse vals)) result)))
                     (else (error "unexpected status " status)))
                   (set! keyword (string->symbol token))
                   (if (eq? action-prefix '*eof*) 'stop 'have-read-keyword))
                 ((#\=)
                   (case status
                     ((have-read-keyword)  ; the first value after the keyword
                       (set! vals (list token))
                       'have-read-value)
                     ((have-read-value)
                       (error "= unexpected after the first value"))
                     (else (error "unexpected status " status))))
                 ((#\+)
                   (case status
                     ((have-read-keyword)
                       (error "+ unexpected after a keyword"))
                     ((have-read-value)		; other values after the keyword
                       (if (equal? (car vals) "")
                         (set-car! vals token)	; if the previous token was empty, ditch it
                         (set! vals (cons token vals)))
                         'have-read-value)
                     (else (error "unexpected status " status))))
                 (else (error "unexpected action-prefix " action-prefix))))))))))
