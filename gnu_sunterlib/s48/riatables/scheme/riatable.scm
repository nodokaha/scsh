;;;;;; Riatables: alternative hash tables                -*- Scheme -*-
;;;;;; Thread-unsafe implementation

;;; Taylor Campbell wrote this code; he places it in the public domain.

(define-record-type table type/table
  (really-make-table id
                     template
                     size
                     gc-stamp
                     bucket-vector)
  table?
  (id              table-id)
  (template        table-template)
  (size            table-size            set-table-size!)
  (gc-stamp        table-gc-stamp        set-table-gc-stamp!)
  (bucket-vector   table-bucket-vector   set-table-bucket-vector!))

(define-record-discloser type/table
  (lambda (table)
    `(,(or (table-type-name table) 'table)
      ,@(cond ((table-id table) => list) (else '()))
      ,(table-size table))))



;;; --------------------
;;; Parameters

;;; These are just guesses.  I ought to fill in better suggestions at
;;; some point, but these will do for now.

;;; The number of buckets to put in freshly created tables.

(define initial-bucket-count 10)

;;; The next number of buckets to have in a table after PREVIOUS.

(define (next-bucket-count previous)
  (+ (* previous 2) 1))

;;; The maximum average number of entries to have in a bucket.

(define maximum-average-bucket-entries 3)



;;; --------------------
;;; Table templates

;;; This code is rather too verbose for my tastes.  At least there can
;;; be a DEFINE-TEMPLATE-ACCESSOR macro to reduce code tedium.

(define-record-type table-template type/table-template
  (really-make-table-template type-name
                              key-predicate
                              key-comparator
                              key-hasher
                              gc-sensitive?
                              head-weak?
                              tail-weak?
                              searcher
                              expander)
  table-template?
  (type-name        table-template-type-name)
  (key-predicate    table-template-key-predicate)
  (key-comparator   table-template-key-comparator)
  (key-hasher       table-template-key-hasher)
  (gc-sensitive?    table-template-gc-sensitive?)
  (head-weak?       table-template-head-weak?)
  (tail-weak?       table-template-tail-weak?)
  (searcher         table-template-searcher)
  (expander         table-template-expander))

(define-record-discloser type/table-template
  (lambda (t)
    `(table-template ,@(cond ((table-template-type-name t) => list)
                             (else '())))))

(define-syntax define-template-accessors
  (syntax-rules ()
    ((define-template-accessors (table template) ...)
     (begin (define (table t)
              (template (table-template t)))
            ...))))

(define-template-accessors
  (table-type-name          table-template-type-name)
  (table-key-predicate      table-template-key-predicate)
  (table-key-comparator     table-template-key-comparator)
  (table-key-hasher         table-template-key-hasher)
  (table-gc-sensitive?      table-template-gc-sensitive?)
  (table-head-weak?         table-template-head-weak?)
  (table-tail-weak?         table-template-tail-weak?)
  (table-searcher           table-template-searcher)
  (table-expander           table-template-expander))

(define (make-table-template key? key= key-hash
                             gc? head-weak? tail-weak?
                             type-name)
  (let* ((rehasher (make-table-rehasher key-hash
                                        gc? head-weak? tail-weak?))
         (hasher (make-table-hasher key-hash gc? rehasher)))
    (really-make-table-template
     type-name
     key? key= key-hash
     gc? head-weak? tail-weak?
     (make-table-searcher key= hasher gc?
                          head-weak? tail-weak?)
     (make-table-expander hasher gc?
                          head-weak? tail-weak?
                          rehasher))))

(define (make-weak-table-template key? key= key-hash gc? type-name)
  (make-table-template key? key= key-hash gc? #t #t type-name))
(define (make-strong-table-template key? key= key-hash gc? type-name)
  (make-table-template key? key= key-hash gc? #f #f type-name))

(define (modify-table-template-weakness template head-weak? tail-weak?)
  (make-table-template (table-template-key-predicate   template)
                       (table-template-key-comparator  template)
                       (table-template-key-hasher      template)
                       (table-template-gc-sensitive?   template)
                       head-weak?
                       tail-weak?
                       (table-template-type-name       template)))

(define-syntax define-weakness-modifiers
  (syntax-rules ()
    ((define-weakness-modifiers
       (name template head? tail?)
       ...)
     (begin (define (name template)
              (modify-table-template-weakness template head? tail?))
            ...))))

(define-weakness-modifiers
  (weaken-table-template           t #t #t)
  (head-weaken-table-template      t #t (table-template-tail-weak? t))
  (tail-weaken-table-template      t (table-template-head-weak? t) #t)
  (strengthen-table-template       t #f #f)
  (head-strengthen-table-template  t #f (table-template-tail-weak? t))
  (tail-strengthen-table-template  t (table-template-head-weak? t) #f))



;;; --------------------
;;; Constructors

(define (%make-table-from-template template suggested-size id)
  (let ((gc? (table-template-gc-sensitive? template)))
    (really-make-table id
                       template
                       0
                       (and gc? (gc-stamp))
                       (make-vector
                        (if (and suggested-size
                                 (> suggested-size
                                    maximum-average-bucket-entries))
                            (quotient suggested-size
                                      maximum-average-bucket-entries)
                            initial-bucket-count)
                        (empty-bucket)))))

(define (table-constructor template)
  (let ((name `(table-constructor ,template)))
    (lambda size+id
      (receive (suggested-size id)
               (table-cons-options size+id name)
        (%make-table-from-template template suggested-size id)))))

(define (make-table-from-template template . size+id)
  (receive (suggested-size id)
           (table-cons-options size+id make-table-from-template)
    (%make-table-from-template template suggested-size id)))

(define (table-cons-options options callee)
  (if (null? options)
      (values #f #f)
      (let ((size (car options))
            (more (cdr options)))
        (if (or (integer? size) (not size))
            (if (null? more)
                (values size #f)
                (let ((name (car more))
                      (more (cdr more)))
                  (if (null? more)
                      (values size name)
                      (error "extraneous arguments" more callee))))
            (error "invalid table size suggestion argument"
                   size
                   callee)))))



;;; --------------------
;;; Main table operations

;;; This is a macro so that it gets integrated and debugging info is
;;; retained about the procedure.

(define-syntax define-keyed-table-operation
  (syntax-rules ()
    ((define-keyed-table-operation (name table-arg key-arg arg ...)
         (bucket-index-var entry-var)
       body1 body2 ...)
     (define (name table-arg key-arg arg ...)
       (cond ((not (table? table-arg))
              (call-error "invalid table argument" name
                          table-arg key-arg arg ...))
             ((not ((table-key-predicate table-arg) key-arg))
              (call-error "invalid key argument" name
                          table-arg key-arg arg ...))
             (else
              (receive (bucket-index-var entry-var)
                       ((table-searcher table-arg)
                        table-arg key-arg)
                body1 body2 ...)))))))

(define-keyed-table-operation (table-entry table key)
    (bucket-index entry)
  (and entry
       (let ((tail-weak? (table-tail-weak? table)))
         (cond ((not tail-weak?)
                (entry-value entry))
               ((weak-entry-value entry))
               (else                    ; Broken weak entry.
                (expunge-entry-from-table! table bucket-index entry)
                #f)))))

(define-keyed-table-operation (set-table-entry! table key value)
    (bucket-index entry)
  (cond ((not value)
         (cond (entry
                (expunge-entry-from-table! table bucket-index entry)
                (if (table-tail-weak? table)
                    (set-weak-entry-value! entry #f)
                    (set-entry-value! entry #f)))))
        (entry
         (if (table-tail-weak? table)
             (set-weak-entry-value! entry #f)
             (set-entry-value! entry #f)))
        (bucket-index
         ((table-expander table) table bucket-index key value))))

(define-keyed-table-operation (modify-table-entry! table key modifier)
    (bucket-index entry)
  (cond (entry
         ;; We don't need to set the entry's value to be #F here: both
         ;; of the modifiers will set the entry to #F if MODIFIER
         ;; returns it.
         (if (not (if (table-tail-weak? table)
                      (modify-weak-entry-value! entry modifier)
                      (modify-entry-value! entry modifier)))
             (expunge-entry-from-table! table bucket-index entry)))
        ((modifier #f)
         => (lambda (new-value)
              ((table-expander table)
               table bucket-index key new-value)))))

(define-keyed-table-operation (pop-table-entry! table key)
    (bucket-index entry)
  (and entry
       (begin (expunge-entry-from-table! table bucket-index entry)
              (if (table-tail-weak? table)
                  (weak-entry-value entry)
                  (entry-value entry)))))

;++ This should also clean out any of the table's broken weak entries.

(define (table->alist table)
  (let* ((buckets (table-bucket-vector table))
         (bcount (vector-length buckets))
         (cons-entry
          (let ((head-weak? (table-head-weak? table))
                (tail-weak? (table-tail-weak? table)))
            (cond ((and head-weak? tail-weak?)
                   (lambda (entry alist)
                     (cond ((weak-entry-key entry)
                            => (lambda (k)
                                 (cond ((weak-entry-value entry)
                                        => (lambda (v)
                                             (cons (cons k v) alist)))
                                       (else alist))))
                           (else alist))))
                  (head-weak?
                   (lambda (entry alist)
                     (cond ((weak-entry-key entry)
                            => (lambda (k)
                                 (cons (cons k (entry-value entry))
                                       alist)))
                           (else alist))))
                  (tail-weak?
                   (lambda (entry alist)
                     (cond ((weak-entry-value entry)
                            => (lambda (v)
                                 (cons (cons (entry-key entry) v)
                                       alist)))
                           (else alist))))
                  (else
                   (lambda (entry alist)
                     (cons (cons (entry-key entry) (entry-value entry))
                           alist)))))))
    (let outer-loop ((alist '())
                     (i 0))
      (if (= i bcount)
          alist
          (let inner-loop ((alist alist)
                           (bucket (vector-ref buckets i)))
            (if (bucket-empty? bucket)
                (outer-loop alist (+ i 1))
                (inner-loop (cons-entry (bucket-entry bucket) alist)
                            (bucket-next bucket))))))))

(define (walk-table table proc)
  (for-each (lambda (k.v) (proc (car k.v) (cdr k.v)))
            (table->alist table)))



;;; --------------------
;;; Buckets

;;; Buckets are represented as alists, because ASSQ in the Scheme48 VM
;;; is fast.  However, this is no longer useful: using ASSQ was an
;;; optimization hack that would lose with optimistic concurrency, so
;;; it is no longer used.  A more space-efficient representation could,
;;; and probably should, be chosen now with no adverse effects on time
;;; efficiency.

;;; All these trivial procedures are procedures, not direct aliases, so
;;; that the silly Scheme48 automatic procedure integrator, which has
;;; much too low a threshold and no provision for alias integration,
;;; will integrate them.

(define (empty-bucket) '())
(define (bucket-empty? bucket) (null? bucket))
(define (bucket-nonempty? bucket) (pair? bucket))

(define (make-bucket entry next) (cons entry next))

(define (bucket-entry bucket) (car bucket))
(define (bucket-next  bucket) (cdr bucket))
(define (set-bucket-next! bucket next) (set-cdr! bucket next))

(define (entry-maker head-weak? tail-weak?)
  (cond ((and head-weak? tail-weak?)
         make-weak-entry)
        (head-weak? make-head-weak-entry)
        (tail-weak? make-tail-weak-entry)
        (else make-strong-entry)))

(define (make-weak-entry key value)
  (cons (make-weak-pointer key)
        (make-weak-pointer value)))
(define (make-head-weak-entry key value)
  (cons (make-weak-pointer key) value))
(define (make-tail-weak-entry key value)
  (cons key (make-weak-pointer value)))
(define (make-strong-entry key value)
  (cons key value))

(define (entry-key entry) (car entry))
(define (weak-entry-key entry) (weak-pointer-ref (car entry)))

(define (entry-value entry) (cdr entry))
(define (weak-entry-value entry) (weak-pointer-ref (cdr entry)))

(define (set-entry-value! entry value)
  (set-cdr! entry value))
(define (set-weak-entry-value! entry value)
  (set-entry-value! entry (make-weak-pointer value)))

;;; If MODIFIER returns #F, the entry is to be deleted.  It would seem
;;; superfluous to set it if it is #F, but the entry, if it is to be
;;; deleted, needs to be set to #F.  This is because of a delicate case
;;; in thread synchronization:
;;;   - Thread A is rehashing a table; it is in the middle of dumping
;;;     the bucket that contains an entry E, which it has already
;;;     dumped.
;;;   - Thread A is suspended; thread B gets to run.
;;;   - Thread B tries to expunge E from the bucket, but it's absent,
;;;     so it assumes that another thread concurrently deleted it.
;;; Now the entry E is still in the table, even though thread B thought
;;; it must have been deleted.

(define (modify-entry-value! entry modifier)
  (let ((new-value (modifier (entry-value entry))))
    (set-entry-value! entry new-value)
    new-value))
(define (modify-weak-entry-value! entry modifier)
  (let ((new-value (modifier (weak-entry-value entry))))
    (set-weak-entry-value! entry new-value)
    new-value))

;;; It might be better to write a purely functional, allocation-only
;;; version of this, to perform as little proposal logging as possible.
;;; Then again, it might make GCs more frequent.
;++ But couldn't that be handled by having two versions, one for GC-
;++ sensitive tables and one for GC-insensitive tables, with the latter
;++ of which allocation has no problems?

(define (expunge-entry-from-bucket! bucket entry)
  (cond ((bucket-empty? bucket) bucket)
        ((eq? entry (bucket-entry bucket))
         (bucket-next bucket))
        (else
         (let loop ((b (bucket-next bucket))
                    (lag bucket))
           ;; It is OK for BUCKET not to contain ENTRY: another thread
           ;; may have concurrently deleted it before the call to
           ;; EXPUNGE-ENTRY-FROM-BUCKET! but after the search for the
           ;; entry.
           (if (bucket-nonempty? b)
               (cond ((eq? (bucket-entry b) entry)
                      (set-bucket-next! lag (bucket-next b))
                      bucket)
                     (else
                      (loop (bucket-next b) b))))))))



;;; --------------------
;;; Bucket operations

(define (make-bucket-searcher key= gc? head-weak? tail-weak?)
  ((cond ((and gc? (or head-weak? tail-weak?))
          make-gc-weak-bucket-searcher)
         ((and head-weak? tail-weak?)
          make-weak-bucket-searcher)
         (head-weak? make-head-weak-bucket-searcher)
         (tail-weak? make-tail-weak-bucket-searcher)
         (else make-strong-bucket-searcher))
   key=))

(define-syntax define-bucket-searcher
  (syntax-rules ()
    ((define-bucket-searcher name
       entry-var
       (key-var key-expression)
       test)
     (define (name key=)
       (letrec ((expunge-broken-initials
                 (lambda (bucket delta)
                   (if (bucket-empty? bucket)
                       (values bucket delta)
                       (let* ((entry-var (bucket-entry bucket))
                              (next (bucket-next bucket))
                              (key-var key-expression))
                         (if test
                             (values bucket delta)
                             (expunge-broken-initials next
                                                      (+ delta 1)))))))
                (search
                 (lambda (bucket lag key delta)
                   (if (bucket-empty? bucket)
                       (values #f delta)
                       (let* ((entry-var (bucket-entry bucket))
                              (next (bucket-next bucket))
                              (key-var key-expression))
                         (cond ((not test)
                                (set-bucket-next! lag next)
                                (search next lag key (+ delta 1)))
                               ((key= key-var key)
                                (values entry-var delta))
                               (else
                                (search next bucket key delta))))))))
         (lambda (table index bucket key)
           (receive (bucket* delta)
                    (expunge-broken-initials bucket 0)
             (if (positive? delta)
                 (vector-set! (table-bucket-vector table)
                              index
                              bucket*))
             (receive (entry delta)
                      (if (bucket-empty? bucket*)
                          (values #f delta)
                          (let ((entry-var (bucket-entry bucket*))
                                (next (bucket-next bucket*)))
                            (if (key= key-expression key)
                                (values entry-var delta)
                                (search next
                                        bucket*
                                        key
                                        delta))))
               (set-table-size! table (- (table-size table) delta))
               entry))))))))

(define-bucket-searcher make-weak-bucket-searcher
  entry
  (key (weak-entry-key entry))
  (and key (weak-entry-value entry)))

(define-bucket-searcher make-head-weak-bucket-searcher
  entry
  (key (weak-entry-key entry))
  key)

(define-bucket-searcher make-tail-weak-bucket-searcher
  entry
  (key (entry-key entry))
  (weak-entry-value entry))

;;; This is just like the strong bucket searcher, except that it looks
;;; at the weak pointer keys' contents, not at the keys themselves.
;;; This works because, if a key was broken at the last GC, the rehash
;;; of the table would have cleared it out before calling this.

(define (make-gc-weak-bucket-searcher key=)
  (lambda (table index bucket key)
    (let loop ((bucket bucket))
      (if (bucket-empty? bucket)
          #f
          (let ((entry (bucket-entry bucket)))
            (if (key= (weak-entry-key entry) key)
                entry
                (loop (bucket-next bucket))))))))

(define (make-strong-bucket-searcher key=)
  ;** EXPUNGE WHEN OPTIMISTIC CONCURRENCY IS IMPLEMENTED
  ;** (ASSQ doesn't log)
  (if (eq? key= eq?)
      (lambda (table index bucket key)  ;+++ Performance hack: ASSQ is
        (assq key bucket))              ;+++ a VM primitive, so fast.
      (letrec ((loop (lambda (table index bucket key)
                       (if (bucket-empty? bucket)
                           #f
                           (let ((entry (bucket-entry bucket)))
                             (if (key= (entry-key entry) key)
                                 entry
                                 (loop table
                                       index
                                       (bucket-next bucket)
                                       key)))))))
        loop)))



(define (make-bucket-dumper key-hash head-weak? tail-weak?)
  ((cond ((and head-weak? tail-weak?)
          make-weak-bucket-dumper)
         (head-weak? make-head-weak-bucket-dumper)
         (tail-weak? make-tail-weak-bucket-dumper)
         (else make-strong-bucket-dumper))
   key-hash))

(define-syntax define-bucket-dumper
  (syntax-rules ()
    ((define-bucket-dumper name
       entry-var
       (key-var key-expression)
       test)
     (define (name key-hash)
       (letrec ((loop
                 (lambda (buckets index bucket count mod)
                   (if (bucket-empty? bucket)
                       count
                       (let* ((entry-var (bucket-entry bucket))
                              (next (bucket-next bucket))
                              (key-var key-expression))
                         (loop buckets index
                               next
                               (if test
                                   (let ((hash (key-hash key-var mod)))
                                     (if (not (eq? hash index))
                                         (begin (set-bucket-next!
                                                 bucket
                                                 (vector-ref buckets
                                                             hash))
                                                (vector-set! buckets
                                                             hash
                                                             bucket)))
                                     (+ count 1))
                                   count)
                               mod))))))
         (lambda (bucket-vector old-buckets index)
           (loop bucket-vector index
                 (vector-ref old-buckets index)
                 0
                 (vector-length bucket-vector))))))))

(define-bucket-dumper make-weak-bucket-dumper
  entry
  (key (weak-entry-key entry))
  (and key (weak-entry-value entry)))

(define-bucket-dumper make-head-weak-bucket-dumper
  entry
  (key (weak-entry-key entry))
  key)

(define-bucket-dumper make-tail-weak-bucket-dumper
  entry
  (key (entry-key entry))
  (weak-entry-value entry))

(define (make-strong-bucket-dumper key-hash)
  (letrec ((loop
            (lambda (buckets index bucket count mod)
              (if (bucket-empty? bucket)
                  count
                  (let ((next (bucket-next bucket))
                        (hash
                         (key-hash (entry-key (bucket-entry bucket))
                                   mod)))
                    ;; Prevent accidentally creating a circular bucket.
                    (if (not (eq? hash index))
                        (begin (set-bucket-next! bucket
                                                 (vector-ref buckets
                                                             hash))
                               (vector-set! buckets hash bucket)))
                    (loop buckets index next (+ count 1) mod))))))
    (lambda (bucket-vector old-buckets index)
      (loop bucket-vector
            index
            (vector-ref old-buckets index)
            0
            (vector-length bucket-vector)))))



;;; --------------------
;;; Internal hash table procedures

(define (make-table-rehasher key-hash gc? head-weak? tail-weak?)
  ((if gc?
       make-gc-table-rehasher
       make-stable-table-rehasher)
   (make-bucket-dumper key-hash head-weak? tail-weak?)))

;;; GC-sensitive table rehashers have to be very carefully constructed.
;;; They may not allocate, and it must be absolutely certain whether or
;;; not they succeeded in hashing immediately following a certain GC
;;; stamp.

(define (make-gc-table-rehasher dump-bucket!)
  (letrec ((attempt-rehash!
            (make-gc-attempted-table-rehasher dump-bucket!))
           (loop (lambda (table new-vector)
                   (if (attempt-rehash! table (gc-stamp) new-vector)
                       #f
                       (loop table new-vector)))))
    (lambda (table stamp new-vector)
      (or (attempt-rehash! table stamp new-vector)
          (loop table new-vector)))))

;;; This returns a procedure that tries to copy all the buckets from
;;; TABLE's existing bucket vector to NEW-VECTOR.

(define (make-gc-attempted-table-rehasher dump-bucket!)
  (define (loop table stamp new-vector old-buckets old-length
                i new-size)
    (cond ((not (eq? stamp (gc-stamp)))
           ;; Immediately abort, even if we finished copying all of the
           ;; buckets, because our efforts were for naught.
           (vector-fill! new-vector (empty-bucket))
           #f)
          ((= i old-length)
           ;; We succeeded in rehashing and a GC didn't occur to
           ;; circumvent us.  Of course, a GC could still occur in the
           ;; next couple instructions, but that will be caught in the
           ;; next table lookup; what is important is that the table
           ;; table was completely rehashed safely.
           (set-table-gc-stamp! table stamp)
           (set-table-bucket-vector! table new-vector)
           (set-table-size! table new-size)
           #t)
          (else
           ;; Copy the bucket at I & proceed happily.  DUMP-BUCKET!
           ;; could test the stamp itself, but it's probably a lot
           ;; cheaper just to test it after every bucket copied.
           (loop table stamp new-vector old-buckets old-length
                 (+ i 1)
                 (+ (dump-bucket! new-vector old-buckets i)
                    new-size)))))
  (lambda (table stamp new-vector)
    (let ((old-buckets (table-bucket-vector table)))
      (loop table stamp new-vector
            old-buckets
            (vector-length old-buckets)
            0 0))))

(define (make-stable-table-rehasher dump-bucket!)
  (lambda (table stamp new-vector)
    (let* ((old-buckets (table-bucket-vector table))
           (old-length  (vector-length old-buckets)))
      (do ((i 0 (+ i 1))
           (new-size 0
                     (+ (dump-bucket! new-vector old-buckets i)
                        new-size)))
          ((= i old-length)
           (set-table-bucket-vector! table
                                     new-vector)
           (set-table-size! table new-size)
           #t)))))

(define (make-table-hasher key-hash gc? rehasher)
  (if gc?
      (make-gc-table-hasher key-hash rehasher)
      (lambda (table key)
        (key-hash key (vector-length (table-bucket-vector table))))))

(define (make-gc-table-hasher key-hash rehash-table!)
  (letrec ((retry
            (lambda (table key mod new-vector)
              (let* ((hash (key-hash key mod))
                     (stamp (gc-stamp)))
                (if (or (eq? (table-gc-stamp table) stamp)
                        (rehash-table! table stamp new-vector))
                    hash
                    (retry table key mod new-vector))))))
    (lambda (table key)
      (let ((mod (vector-length (table-bucket-vector table))))
        (let ((stamp (gc-stamp)))
          (if (eq? (table-gc-stamp table) stamp)
              (let* ((hash (key-hash key mod)))
                (if (eq? (gc-stamp) stamp)
                    hash
              ;;; These next two calls are duplicated, but it is
              ;;; needed so we don't allocate unnecessarily.
                    (retry table key mod
                           (make-vector mod (empty-bucket)))))
              (retry table key mod
                     (make-vector mod (empty-bucket)))))))))

(define (make-table-searcher key= table-hash gc? head-weak? tail-weak?)
  (let ((search-bucket (make-bucket-searcher key= gc?
                                             head-weak? tail-weak?)))
    (lambda (table key)
      (let* ((hash (table-hash table key))
             (bucket-vector (table-bucket-vector table))
             (bucket (vector-ref bucket-vector hash)))
        (values hash (search-bucket table hash bucket key))))))

(define (make-table-expander table-hash gc?
                             head-weak? tail-weak?
                             rehash-table!)
  (let ((make-entry (entry-maker head-weak? tail-weak?))
        (rehash! (if gc?
                     (lambda (table new-vector)
                       (let loop ((stamp (table-gc-stamp table)))
                         (if (rehash-table! table stamp new-vector)
                             (values)
                             (loop (gc-stamp)))))
                     (lambda (table new-vector)
                       (rehash-table! table #f new-vector)))))
    (lambda (table cached-hash key value)
      (let* ((hash (if (maybe-expand-table! table rehash!)
                       cached-hash
                       (table-hash table key)))
             ;; Note the LET*: the rehash might set the table's bucket
             ;; vector.
             (bucket-vector (table-bucket-vector table)))
        (vector-set! bucket-vector hash
                     (make-bucket (make-entry key value)
                                  (vector-ref bucket-vector hash)))
        (set-table-size! table (+ (table-size table) 1))))))

(define (maybe-expand-table! table rehash!)
  (let loop ((first? #t))
    (let ((size (table-size table)))
      (cond ((<= size (* (vector-length (table-bucket-vector table))
                         maximum-average-bucket-entries))
             first?)
            (else
             (rehash! table
                      (make-vector (next-bucket-count size)
                                   (empty-bucket)))
             (loop #f))))))

(define (expunge-entry-from-table! table bucket-index entry)
  (let ((bucket-vector (table-bucket-vector table)))
    (vector-set! bucket-vector bucket-index
                 (expunge-entry-from-bucket!
                  (vector-ref bucket-vector bucket-index)
                  entry))))



;;; --------------------
;;; Various table templates

(define-syntax define-table-types
  (syntax-rules ()
    ((define-table-types
       (template-name constructor-name template-exp)
       ...)
     (begin (begin (define template-name template-exp)
                   (define constructor-name
                           (table-constructor template-name)))
            ...))))

(define-table-types
  (object-table-template make-object-table
    (make-strong-table-template (lambda (x) #t) eq?
                                modular-descriptor-hash
                                #t      ; GC-sensitive hash function
                                'object-table))
  (weak-object-table-template make-weak-object-table
    (make-weak-table-template (lambda (x) #t) eq?
                              modular-descriptor-hash
                              #t        ; GC-sensitive hash function
                              'weak-object-table))
  (string-table-template make-string-table
    (make-strong-table-template string? string=?
                                modular-string-hash
                                #f      ; GC-insensitive hash function
                                'string-table))
  (string-ci-table-template make-string-ci-table
    (make-strong-table-template string? string-ci=?
                                modular-string-ci-hash
                                #f      ; GC-insensitive hash function
                                'string-ci-table))
  (integer-table-template make-integer-table
    (make-strong-table-template integer? =
                                modular-integer-hash
                                #f      ; GC-insensitive hash function
                                'integer-table))
  (symbol-table-template make-symbol-table
    (make-strong-table-template symbol? eq?
                                (lambda (sym mod)
                                  (modular-string-hash
                                   (symbol->string sym)
                                   mod))
                                #f      ; GC-insensitive hash function
                                'symbol-table)))
