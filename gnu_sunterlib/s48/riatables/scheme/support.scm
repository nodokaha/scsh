;;;;;; Riatables: alternative hash tables                -*- Scheme -*-
;;;;;; Miscellaneous support utilities

;;; Taylor Campbell wrote this code; he places it in the public domain.

(define (gc-stamp)
  (memory-status (enum memory-status-option gc-count) 0))

(define (descriptor-hash object)
  (memory-status (enum memory-status-option pointer-hash) object))

(define (modular-descriptor-hash object mod)
  (modulo (descriptor-hash object) mod))

;;; Silly string hashers.  I use Scheme48's STRING-HASH, not a better
;;; algorithm (Scheme48's is pretty terrible), because it's a primitive
;;; in the VM and therefore faster than anything in plain Scheme.

(define (modular-string-hash string mod)
  (modulo (string-hash string) mod))

(define (string-ci-hash string)
  (let* ((length (string-length string))
         (new-string (make-string length)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (string-set! new-string i (char-downcase (string-ref string i))))
    (string-hash string)))

(define (modular-string-ci-hash string mod)
  (modulo (string-ci-hash string) mod))

(define (integer-hash integer) (abs integer))

(define (modular-integer-hash integer mod)
  (modulo integer mod))

(define (exact-integer? x)
  (and (integer? x) (exact? x)))
