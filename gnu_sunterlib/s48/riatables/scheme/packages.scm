;;;;;; Riatables: alternative hash tables                -*- Scheme -*-
;;;;;; Interface and package definitions

;;; Taylor Campbell wrote this code; he places it in the public domain.

(define-interface riatables-interface
  (export table-template?
          make-table-template
          make-weak-table-template
          make-strong-table-template

               weaken-table-template          strengthen-table-template
          head-weaken-table-template     head-strengthen-table-template
          tail-weaken-table-template     tail-strengthen-table-template
          modify-table-template-weakness

          table-template-key-predicate
          table-template-key-comparator
          table-template-key-hasher
          table-template-gc-sensitive?
          table-template-head-weak?
          table-template-tail-weak?

          table-constructor

          table?
          make-table-from-template

          table-size
          table-entry
          set-table-entry!
          modify-table-entry!
          pop-table-entry!
          walk-table
          table->alist

          table-template

          descriptor-hash       modular-descriptor-hash
          string-hash           modular-string-hash
          string-ci-hash        modular-string-ci-hash
          integer-hash          modular-integer-hash

          make-object-table             object-table-template
          make-weak-object-table   weak-object-table-template
          make-string-table             string-table-template
          make-string-ci-table       string-ci-table-template
          make-integer-table           integer-table-template
          make-symbol-table             symbol-table-template))

(define-interface riatables-support-interface
  (export gc-stamp
          descriptor-hash	modular-descriptor-hash
          string-hash		modular-string-hash
          string-ci-hash	modular-string-ci-hash
          integer-hash		modular-integer-hash
          exact-integer?))

(define-structure riatables riatables-interface
  (open scheme
        receiving
        weak
        (subset signals			(error call-error))
        define-record-types
        riatables-support)
  (optimize auto-integrate)
  (files riatable))

(define-structure riatables-support riatables-support-interface
  (open scheme
        (subset primitives		(memory-status string-hash))
        (subset architecture		(memory-status-option))
        enumerated)
  (optimize auto-integrate)
  (files support))
