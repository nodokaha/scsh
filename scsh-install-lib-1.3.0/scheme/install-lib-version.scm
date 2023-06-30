;;
;; Version of the installation library
;;
;; The versioning scheme is as follows: a version is composed of three
;; integers called (from left to right) "major", "minor" and
;; "revision".
;;
;; Two versions which share a "major" and "minor" number must be fully
;; compatible in that one should be exchangeable for the other without
;; (important) change in behaviour.
;;
;; Two versions which share a "major" number must be compatible in an
;; ascendent fashion: the features offered by the version with the
;; greatest "minor" number must be a superset of those offered by the
;; other.
;;
;; Two versions which do not even share a "major" number can be
;; mutually incompatible.
;;
;; Clients using the installation library must specify which "major"
;; and "minor" number they need --- if the above scheme is respected,
;; the "revision" should not matter. This need is satisfied if the
;; requested "major" number matches the one of the library, and the
;; requested "minor" is smaller or equal to the one of the library.

(define install-lib-version '(1 3 0))
