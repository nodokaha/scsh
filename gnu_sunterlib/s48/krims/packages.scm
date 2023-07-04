; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;; since SRFI-1-INTERFACE isn't defined in the usual 0.6.3 image
;; definition hijacked from scsh-0.6.3/scheme/more-interfaces.scm
(define-interface srfi-1-face
  (export map for-each member assoc	; redefined from R5RS
	  xcons make-list list-tabulate cons* list-copy
	  proper-list? circular-list? dotted-list? not-pair? null-list? list=
	  circular-list length+
	  iota
	  first second third fourth fifth sixth seventh eighth ninth tenth
	  car+cdr
	  take       drop
	  take-right drop-right
	  take!      drop-right!
	  split-at   split-at!
	  last last-pair
	  zip unzip1 unzip2 unzip3 unzip4 unzip5
	  count
	  append! append-reverse append-reverse! concatenate concatenate!
	  unfold       fold       pair-fold       reduce
	  unfold-right fold-right pair-fold-right reduce-right
	  append-map append-map! map! pair-for-each filter-map map-in-order
	  filter  partition  remove
	  filter! partition! remove!
	  find find-tail any every list-index
	  take-while drop-while take-while!
	  span break span! break!
	  delete delete!
	  alist-cons alist-copy
	  delete-duplicates delete-duplicates!
	  alist-delete alist-delete!
	  reverse!
	  lset<= lset= lset-adjoin
	  lset-union  lset-intersection  lset-difference  lset-xor
	  lset-diff+intersection
	  lset-union! lset-intersection! lset-difference! lset-xor!
	  lset-diff+intersection!))

(define-interface onebol-face
  (export fail error
          (assert :syntax)
          (deny :syntax)
          should-raise*
          shouldnt-raise*
          (should-raise :syntax)
          (shouldnt-raise :syntax)
          ;; should we export more or less condition predicates?
          failure?
          error?                        ; imported from conditions
          ))


;; odds and ends
(define-structure krims
  (export (assert :syntax)
          (receive/name :syntax)
          (gen-dispatch :syntax))
  (open srfi-28                         ; format
        srfi-23                         ; error
        scheme)
  (files krims))

;; srfi-1 + REST
(define-structure srfi-1+
  (compound-interface srfi-1-face
                      (export rest))
  (open srfi-1 scheme)
  (begin (define rest cdr)))

;; srfi-9 + define-record-discloser
;; [ extended version of the srfi-9 structure def
;;   from scsh-0.6.3/scheme/more-packages.scm ]
(define-structure srfi-9+
  (export (define-record-type :syntax)
          define-record-discloser)
  (open scheme-level-2
	(with-prefix define-record-types sys:))
  (begin
    (define-syntax define-record-type
      (syntax-rules ()
	((define-record-type type-name . stuff)
	 (sys:define-record-type type-name type-name . stuff))))
    (define define-record-discloser sys:define-record-discloser)))


;; open
;; ,open handle conditions signals srfi-9
;; open (subset handle (with-handler))

(define-structure onebol onebol-face
  (open (subset handle (with-handler))
        conditions
        signals
        srfi-9
        scheme)
  (files onebol))

;; run stuff from test.scm ,in onebol-testbed
(define-structure onebol-testbed (export )
  (open onebol scheme)
  (files ))
