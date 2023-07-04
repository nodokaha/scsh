;;; Functions to manipulate integer sets represented as lists of
;;; intervals.
;;;
;;; Sets are represented as lists of intervals, which are pairs (lower
;;; bound, upper bound), where both bounds are included. The lower
;;; bound must be an integer, the upper bound can either be an integer
;;; or the symbol 'max' to specify the maximum possible value of all
;;; intervals for the problem at hand. The specific value of this
;;; maximum is not known.
;;;
;;; The following implementation guarantees that sets are always in
;;; canonical form, that is their intervals are:
;;;   - sorted in increasing order,
;;;   - disjoint, and
;;;   - non-contiguous (i.e. they do not touch each other).
;;;
;;; External dependencies: SRFI-1 (list library) and SRFI-23 (error).

(define (pairwise f l)
  (or (< (length l) 2)
      (and (f (first l) (second l)) (pairwise f (cdr l)))))

;;; Functions on bounds.

(define (b? thing) (or (number? thing) (b-max? thing)))

(define (b-max? bound) (eq? bound 'max))

(define (b< b1 b2)
  (and (not (b-max? b1)) (or (b-max? b2) (< b1 b2))))

(define (b<= b1 b2)
  (or (eq? b1 b2) (b< b1 b2)))

(define (bs<= . bounds)
  (pairwise b<= bounds))

(define (b-pred bound)
  (if (b-max? bound) (error "no predecessor to 'max'") (- bound 1)))

(define (b-succ bound)
  (if (b-max? bound) (error "no successor to 'max'") (+ bound 1)))

;; Saturating successor.
(define (b-sat-succ bound)
  (if (b-max? bound) bound (+ bound 1)))

(define (b-min b1 b2)
  (cond ((b-max? b1) b2)
        ((b-max? b2) b1)
        (else (min b1 b2))))

(define (b-max b1 b2)
  (cond ((b-max? b1) b1)
        ((b-max? b2) b2)
        (else (max b1 b2))))

;;; Functions on individual intervals (pairs of bounds).

(define i-make cons)
(define i-beg car)
(define i-end cdr)

(define (i? thing)
  (and (pair? thing)
       (let ((b1 (car thing)) (b2 (cdr thing)))
         (and (b? b1)
              (b? b2)
              (b<= b1 b2)))))

;; Beware: the following syntax leads to multiple evaluations of each
;; interval expression!
(define-syntax let-int
  (syntax-rules ()
    ((let-int ((beg-1 end-1 int-1) rest ...) body ...)
     (let ((beg-1 (i-beg int-1)) (end-1 (i-end int-1)))
       (let-int (rest ...) body ...)))
    ((let-int () body ...)
     (begin body ...))))

(define (i-intersect? i1 i2)
  (let-int ((b1 e1 i1) (b2 e2 i2))
           (or (bs<= b1 b2 e1) (bs<= b2 b1 e2))))

(define (i-contiguous? i1 i2)
  (let-int ((b1 e1 i1) (b2 e2 i2))
           (or (bs<= b1 b2 (b-sat-succ e1)) (bs<= b2 b1 (b-sat-succ e2)))))

;; Defined only for contiguous intervals.
(define (i-union i1 i2)
  (let-int ((b1 e1 i1) (b2 e2 i2))
           (i-make (b-min b1 b2) (b-max e1 e2))))

(define (i-start-before? i1 i2)
  (b< (i-beg i1) (i-beg i2)))

(define (i-end-before? i1 i2)
  (b< (i-end i1) (i-end i2)))

;; Defined only for disjoint intervals.
(define i< i-start-before?)

(define (i-intersection i1 i2)
  (if (i-intersect? i1 i2)
      (list (let-int ((b1 e1 i1) (b2 e2 i2))
                     (i-make (b-max b1 b2) (b-min e1 e2))))
      '()))

(define (i-difference i1 i2)
  (if (i-intersect? i1 i2)
      (let-int ((b1 e1 i1) (b2 e2 i2))
               (let ((il (if (b< b1 b2) (list (i-make b1 (b-pred b2))) '()))
                     (ir (if (b< e2 e1) (list (i-make (b-succ e2) e1)) '())))
                 (append il ir)))
      (list i1)))

;;; Functions on sets (lists of individual intervals).

(define (intset? thing)
  (and (proper-list? thing)
       (pairwise (lambda (i1 i2)
                   (and (i? i1) (i? i2)
                        (not (i-contiguous? i1 i2))
                        (i< i1 i2)))
                 thing)))

(define (intset-union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((h1 (car s1)) (t1 (cdr s1))
               (h2 (car s2)) (t2 (cdr s2)))
           (cond ((i-contiguous? h1 h2)
                  (if (i-end-before? h1 h2)
                      (intset-union t1 (cons (i-union h1 h2) t2))
                      (intset-union (cons (i-union h1 h2) t1) t2)))
                 ((i< h1 h2)
                  (cons h1 (intset-union t1 s2)))
                 (else                  ;(i< h2 h1)
                  (cons h2 (intset-union s1 t2))))))))

(define (intset-intersection s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((h1 (car s1)) (t1 (cdr s1))
            (h2 (car s2)) (t2 (cdr s2)))
        (if (i-end-before? h1 h2)
            (append (i-intersection h1 h2) (intset-intersection t1 s2))
            (append (i-intersection h1 h2) (intset-intersection s1 t2))))))

(define (intset-difference s1 s2)
  (if (or (null? s1) (null? s2))
      s1
      (let ((h1 (car s1)) (t1 (cdr s1))
            (h2 (car s2)) (t2 (cdr s2)))
        (cond ((i-intersect? h1 h2)
               (intset-difference (append (i-difference h1 h2) t1) s2))
              ((i< h1 h2)
               (cons h1 (intset-difference t1 s2)))
              (else
               (intset-difference s1 t2))))))

(define (intset-range begin end)
  `((,begin . ,end)))

(define (intset-singleton elem)
  (intset-range elem elem))

(define (intset-adjoin elem set)
  (intset-union set (intset-singleton elem)))

(define (intset-delete elem set)
  (intset-difference set (intset-singleton elem)))

(define (intset-contains? elem set)
  (any (lambda (i) (bs<= (i-beg i) elem (i-end i))) set))

(define (intset-map f set)
  (if (null? set)
      '()
      (let ((fst (car set)))
        (cons (f (car fst) (cdr fst)) (intset-map f (cdr set))))))
