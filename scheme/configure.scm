(define (host) "x86_64-pc-linux-gnu")

(define (machine-vendor-os)
  (let ((match (regexp-search (rx (submatch (+ (~ #\-))) "-"
                                  (submatch (+ (~ #\-))) "-"
                                  (submatch (+ any)))
                              (host))))
    (list (match:substring match 1)
          (match:substring match 2)
          (match:substring match 3))))

(define (machine)
  (car (machine-vendor-os)))

(define (vendor)
  (cadr (machine-vendor-os)))

(define (os)
  (caddr (machine-vendor-os)))

(define (prefix) "/usr/local")

(define (exec-prefix) "${prefix}")

(define (bin-dir) "${exec_prefix}/bin")

(define (lib-dir) "${exec_prefix}/lib")

(define (include-dir) "${prefix}/include")

(define (man-dir) "${prefix}/share/man")

(define (lib-dirs-list) (quote @lib_dirs_list@))

(define (libs) "")

(define (defs) "-DHAVE_CONFIG_H")

(define (cflags) "-g -O2")

(define (cppflags) " -I/usr/local/include")

(define (ldflags) " -rdynamic")

(define (compiler-flags)
  (string-join (list "-I" (include-dir) (defs))))

(define (linker-flags)
  (string-join (list "-L" (lib-dir) (libs) "-lscsh") " "))

;;; Local Variables: 
;;; mode: Scheme
;;; End: 
