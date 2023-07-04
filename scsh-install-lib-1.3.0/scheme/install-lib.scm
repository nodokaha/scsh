;;; Installation library for scsh modules.
;;; $Id: install-lib.scm,v 1.25 2005/08/20 15:45:02 michel-schinz Exp $

;; TODO
;; - think about --build: does it make sense?
;; - get-directory should get 'install or 'final instead of #f #t
;; - add a "--debug" option
;; - add support for communication between configure and pkg-def.scm
;; - add support for image creation
;; - add support to maintain a documentation index

;;
;; Support code templates
;;
;; These templates are meant to be inserted in package-loading
;; scripts.

;; Template to parse libtool's ".la" files.
(define tmpl-libtool-la-reader
  '((define (normalize-la-entry key val)
      (let ((left-quotes-rx (rx (: bos #\')))
            (right-quotes-rx (rx (: #\' eos)))
            (kill-matches
             (lambda (rx str)
               (regexp-substitute/global #f rx str 'pre 'post))))
        (cons (string->symbol key)
              (kill-matches left-quotes-rx
                            (kill-matches right-quotes-rx val)))))
    (define add-la-entry
      (let ((splitter (infix-splitter (rx #\=)))
            (comment-rx (rx (: bos #\#))))
        (lambda (line alist)
          (cond
           ((and (not (regexp-search? comment-rx line))
                 (string-index line #\=))
            (let ((lst (splitter line)))
              (if (= 2 (length lst))
                  (cons (apply normalize-la-entry lst) alist)
                  (error "Could not read la entry" line list))))
           (else alist)))))
    (define (read-libtool-la file-name)
      (call-with-input-file
          file-name
        (lambda (port)
          (let lp ((line (read-line port)) (alist '()))
            (if (eof-object? line)
                alist
                (lp (read-line port) (add-la-entry line alist)))))))))

;;
;; Utilities
;;

(define default-perms (integer->file-mode #o755))

;; Function composition (for functions with one argument)
(define (compose f g) (lambda (x) (f (g x))))

;; Fail if CONDITION is not true, displaying ERROR-MSG with ARGUMENTS.
(define (assert condition error-msg . arguments)
  (if (not condition)
      (apply error error-msg arguments)))

;; True iff LIST has more than one element.
(define (many? list) (> (length list) 1))

;; Return the name of the parent directory of FNAME.
(define (parent-directory fname)
  (file-name-directory (directory-as-file-name fname)))

(define (get-perms integer/fun fname)
  (cond ((procedure? integer/fun) (integer/fun fname))
        ((file-mode? integer/fun) integer/fun)
        (else (error "invalid permission specification" integer/fun))))

;; Create directory FNAME and all its parents, as needed.
(define (create-directory&parents fname . rest)
  (let-optionals rest ((perms default-perms))
    (let ((parent (parent-directory fname)))
      (if (file-not-exists? parent)
          (apply create-directory&parents parent rest))
      (if (file-not-exists? fname)
          (create-directory fname
                            (get-perms perms (absolute-file-name fname)))))))

;; Return the length of the longest prefix common to lists L1 and L2,
;; by comparing elements using PRED (defaults to EQUAL?).
(define (common-prefix-length l1 l2 . rest)
  (let-optionals rest ((pred equal?))
    (if (or (null? l1) (null? l2) (not (pred (first l1) (first l2))))
        0
        (+ 1 (apply common-prefix-length (cdr l1) (cdr l2) rest)))))

;; Return the name of file NAME relative to DIR (defaults to current
;; directory).
(define (relative-file-name name . rest)
  (let-optionals rest ((dir (cwd)))
    (let* ((abs-pl (split-file-name (absolute-file-name name)))
           (dir-pl (split-file-name (directory-as-file-name dir)))
           (cp-len (common-prefix-length abs-pl dir-pl)))
      (path-list->file-name (append (make-list (- (length dir-pl) cp-len) "..")
                                    (drop abs-pl cp-len))))))

;; Similar to path-list->file-name, but take all arguments as
;; components of the path.
(define (paths->file-name . paths)
  (path-list->file-name paths))

;; If FILE exists, fail if --force was not given, delete it otherwise.
(define (delete-file-or-fail file)
  (if (file-exists? file)
      (if (get-option-value 'force)
          (-delete-file file)
          (display-error-and-exit "target file already exists: " file))))

;; Copy file/symlink SOURCE to TARGET. TARGET must be the name of a
;; non-existing file (i.e. it cannot be the name of a directory).
(define (copy-file source target)
  (delete-file-or-fail target)
  (if (file-symlink? source)
      (create-symlink (read-symlink source) target)
      (begin
        (run (cp ,source ,target))
        (set-file-mode (open-input-file target) (file:mode source)))))

;; Like "load" but without printing anything.
(define load-quietly
  (let ((eval (lambda (expr . t) (eval expr (interaction-environment)))))
    (lambda (file-name)
      (call-with-input-file file-name
        (lambda (port) (port-fold port read eval #f))))))

;; Load the contents of FILE-NAME which should contain exactly one
;; s-expression. Complain if it is not the case.
(define (load-single-sexp file-name)
  (if (file-not-exists? file-name)
      (display-error-and-exit "file "file-name" doesn't exist")
      (call-with-input-file file-name
        (lambda (port)
          (read-single-sexp port (string-append "file "file-name))))))

;; Read the contents of PORT which should contain exactly one
;; s-expression. Complain if it is not the case, using SOURCE to
;; identify the source of data.
(define (read-single-sexp port source)
  (let ((sexp (read port)))
    (if (eof-object? sexp)
        (display-error-and-exit source" is empty"))
    (if (not (eof-object? (read port)))
        (display-error-and-exit
         "more than one s-expression found in "source))
    sexp))

(define (permissions->string perms)
  (let ((decode (lambda (mask str)
                  (if (zero? (bitwise-and (file-mode->integer perms) mask)) "-" str))))
    (string-append (decode #o400 "r") (decode #o200 "w") (decode #o100 "x")
                   (decode #o040 "r") (decode #o020 "w") (decode #o010 "x")
                   (decode #o004 "r") (decode #o002 "w") (decode #o001 "x"))))

;; Replace all bindings of KEY in ALIST with one binding KEY to DATUM.
(define (alist-replace key datum alist)
  (alist-cons key datum (alist-delete key alist)))

;; Add all mappings from ALIST-2 to ALIST-1. If a key is mapped in
;; both lists, the mapping in the first list takes precedence.
(define (alist-combine alist-1 alist-2)
  (fold (lambda (key/value result)
          (if (assoc (car key/value) result) result (cons key/value result)))
        alist-1
        alist-2))

;; Return the value associated with KEY in ALIST. If none exists,
;; return DEFAULT, or signal an error if no DEFAULT was given.
(define (alist-get key alist . rest)
  (cond ((assoc key alist) => cdr)
        ((not (null? rest)) (first rest))
        (else (error "cannot find key in alist" key alist))))

;; Display all arguments on the current output port.
(define (display-all . args)
  (for-each display args))

;; Convert all arguments to strings using DISPLAY and concatenate the
;; result in a single string which is returned.
(define (as-string . args)
  (call-with-string-output-port
   (lambda (port) (with-current-output-port port (apply display-all args)))))

;; Return a string of N white spaces.
(define (spaces n) (make-string n #\space))

;;
;; Support for dry runs / verbose operation.
;;

(define (wrap real-fn info-fn)
  (lambda args
    (if (or (get-option-value 'verbose) (get-option-value 'dry-run))
        (begin (display (apply info-fn args)) (newline)))
    (if (not (get-option-value 'dry-run))
        (apply real-fn args))))

(define -create-directory
  (wrap create-directory
        (lambda (fname . rest)
          (let-optionals rest ((perms #o777))
            (as-string "creating directory " fname
                       " (perms: " (permissions->string perms) ")")))))

(define -create-directory&parents
  (wrap create-directory&parents
        (lambda (fname . rest)
          (as-string "creating directory " fname " (and its parents)"))))

(define -create-symlink
  (wrap create-symlink
        (lambda (old-name new-name)
          (as-string "creating symbolic link " new-name
                     " pointing to " old-name))))

(define -copy-file
  (wrap copy-file
        (lambda (source target)
          (as-string "copying file " source " to " target))))

(define -delete-file
  (wrap delete-file
        (lambda (fname) (as-string "deleting file " fname))))

(define -call-with-output-file
  (wrap call-with-output-file
        (lambda (fname port-function)
          (let ((string-out (make-string-output-port)))
            (port-function string-out)
            (as-string "writing>>\n"
                       (string-output-port-output string-out)
                       "<<to file " fname)))))

(define -set-file-mode
  (wrap set-file-mode
        (lambda (fname mode)
          (as-string "setting permissions of " fname " to "
                     (permissions->string mode)))))
;;
;; Versions
;;
;; Versions are represented as lists of integers, the most significant
;; being at the head.

;; Return major/minor parts of a version.
(define version-major first)
(define version-minor second)

;; Return true iff OBJECT can be interpreted as a version.
(define (version? object)
  (and (list? object) (every integer? object)))

;; Return the printed representation of VERSION.
(define (version->string version)
  (string-join (map number->string version) "."))

;; Convert the printed representation of a version found in
;; VERSION-STRING to the version it represents.
(define string->version
  (let ((split-version (infix-splitter ".")))
    (lambda (version-string)
      (map string->number (split-version version-string)))))

;; Compare two versions lexicographically and return the symbol
;; 'smaller if the first is strictly smaller than the second, 'equal
;; if both are equal, and 'greater otherwise.
(define (version-compare v1 v2)
  (cond ((and (null? v1) (null? v2)) 'equal)
        ((null? v1) 'smaller)
        ((null? v2) 'greater)
        (else (let ((v1h (car v1)) (v2h (car v2)))
                (cond ((< v1h v2h) 'smaller)
                      ((> v1h v2h) 'greater)
                      (else (version-compare (cdr v1) (cdr v2))))))))

(define (version<? v1 v2) (eq? (version-compare v1 v2) 'smaller))
(define (version>? v1 v2) (eq? (version-compare v1 v2) 'greater))
(define (version=? v1 v2) (eq? (version-compare v1 v2) 'equal))

;;
;; Phases
;;

(define all-phases '(build build-clean install))

(define all-phases-str (string-join (map symbol->string all-phases) ", "))

(define (valid-phase? thing)
  (member thing all-phases))

(define (phase-active? phase)
  (assert (valid-phase? phase) "invalid phase" phase)
  (member phase (get-option-value 'phases)))

(define (parse-phase str)
  (let ((phase (string->symbol str)))
    (if (valid-phase? phase)
        phase
        (display-error-and-exit "invalid phase "str" "
                                "(valid ones are: "all-phases-str")"))))

(define (parse-phases str)
  (map parse-phase ((infix-splitter ",") str)))

;;
;; Layouts
;;

;; Names of all shared locations (i.e. the ones which do not depend on
;; the platform).
(define shared-locations
  '(active base misc-shared scheme doc))

;; Names of all non-shared (i.e. platform-dependent) locations.
(define non-shared-locations
  '(lib bin))

;; All locations defined for a layout.
(define all-locations (append shared-locations non-shared-locations))

;; Return true iff LOCATION is valid.
(define (valid-location? location)
  (member location all-locations))

;; Return true iff LOCATION is "active", that is if files should be
;; installed in it.
(define (active-location? location)
  (assert (valid-location? location) "invalid location" location)
  (and (phase-active? 'install)
       (or (not (get-option-value 'non-shared-only))
           (member location non-shared-locations))))

;; Parse a layout given as an s-expression evaluating to a procedure.
(define (parse-layout str)
  (let ((layout (eval (read-single-sexp (make-string-input-port str)
                                        "--layout argument")
                      (interaction-environment))))
    (if (procedure? layout)
        layout
        (display-error-and-exit
         "--layout argument doesn't specify a valid layout"
         layout))))

;; Load layout from FILE-NAME, which must contain exactly one
;; s-expression evaluating to a procedure.
(define (load-layout file-name)
  (let ((layout (eval (load-single-sexp file-name) (interaction-environment))))
    (if (procedure? layout)
        layout
        (display-error-and-exit
         "file "file-name" doesn't contain a valid layout"))))

;; Return an absolute version of LAYOUT by prepending PREFIX to all
;; its components (which must be relative).
(define (absolute-layout layout prefix)
  (map (lambda (key/value)
         (cons (car key/value) (absolute-file-name (cdr key/value) prefix)))
       layout))

;; Return the directory associated with the LOCATION in LAYOUT.
(define (layout-dir layout location)
  (assert (valid-location? location) "invalid location" location)
  (or (alist-get location layout #f)
      (display-error-and-exit "cannot find the directory for location '"
                              location "' in active layout.")))

;; Predefined layouts

;; Directory corresponding to the current major and minor version of
;; scsh.
(define scsh-version-string
  (version->string (list scsh-major-version scsh-minor-version)))

(define (scsh-layout base)
  `((base        . ,base)
    (misc-shared . ,base)
    (scheme      . ,(absolute-file-name "scheme" base))
    (lib         . ,(absolute-file-name "lib" base))
    (doc         . ,(absolute-file-name "doc" base))))

(define (scsh-layout-1 pkg)
  (alist-combine `((active . ,scsh-version-string))
                 (scsh-layout (paths->file-name scsh-version-string
                                                (package-full-name pkg)))))

(define (scsh-layout-2 pkg)
  (alist-combine
   `((active . ,(paths->file-name scsh-version-string "active")))
   (scsh-layout (paths->file-name scsh-version-string
                                  "installed"
                                  (package-name pkg)
                                  (version->string (package-version pkg))))))

(define (fhs-layout pkg)
  (let* ((scsh-version-dir (string-append "scsh-" scsh-version-string))
         (base (paths->file-name "share"
                                 scsh-version-dir
                                 "modules"
                                 (package-full-name pkg))))
    `((base        . ,base)
      (misc-shared . ,base)
      (scheme      . ,(absolute-file-name "scheme" base))
      (lib         . ,(paths->file-name "lib"
                                        scsh-version-dir
                                        "modules"
                                        (package-full-name pkg)))
      (doc         . ,(paths->file-name "share"
                                        "doc"
                                        scsh-version-dir
                                        (package-full-name pkg)))
      (active      . ,(paths->file-name "share"
                                        scsh-version-dir
                                        "modules")))))

(define (fhs-program-layout pkg)
  (let* ((pkg-share (paths->file-name "share" (package-full-name pkg))))
    `((bin        . "bin")
      (scheme     . ,(paths->file-name pkg-share "scheme"))
      (doc        . ,(paths->file-name "share"
                                       "doc"
                                       (package-full-name pkg))))))

(define predefined-layouts
  `(("scsh"        . ,scsh-layout-1)
    ("scsh-alt"    . ,scsh-layout-2)
    ("fhs"         . ,fhs-layout)
    ("fhs-program" . ,fhs-program-layout)))

;; If LAYOUT-SPEC is a procedure, return it as-is. If it's a string
;; and refers to a predefined layout, this layout is returned. If it's
;; a string and a valid layout definition, parse and return it.
;; Otherwise, return false.
(define (resolve-layout layout-spec)
  (or (and (procedure? layout-spec) layout-spec)
      (alist-get layout-spec predefined-layouts #f)
      (parse-layout layout-spec)))

;;
;; Packages
;;

(define-record-type package
  (make-package name version extensions directory install-thunk)
  package?
  (name package-name)
  (version package-version)
  (extensions package-extensions)
  (directory package-directory)
  (install-thunk package-install-thunk))

;; Return the full name of PKG.
(define (package-full-name pkg)
  (string-append
   (package-name pkg) "-" (version->string (package-version pkg))))

;; Return the value of extension called EXT for PKG. If such an
;; extension doesn't exist, return #f.
(define (package-extension pkg ext)
  (alist-get ext (package-extensions pkg) #f))

;; List of all defined packages
(define *packages* (make-fluid (make-cell '())))

;; Add PKG to the above list of all defined packages.
(define (add-package pkg)
  (cell-set! (fluid *packages*)
             (cons pkg (cell-ref (fluid *packages*)))))

(define-syntax define-package
  (syntax-rules ()
    ((define-package name version extensions body ...)
     (add-package (make-package name
                                (quasiquote version)
                                (quasiquote extensions)
                                (cwd)
                                (lambda () body ...))))))

(define-syntax define-program
  (syntax-rules ()
    ((define-program name version extensions body ...)
     (define-package name version extensions body ...))))

(define (package-version-requirement pkg)
  (and-let* ((req-lst (package-extension pkg 'install-lib-version)))
            (first req-lst)))

;; Load (and evaluate the contents of) the file "pkg-def.scm" in the
;; current directory and return the packages it defines.
(define (load-packages)
  (let-fluid *packages* (make-cell '())
             (lambda ()
               (if (file-exists? package-definition-file)
                   (load-quietly package-definition-file))
               (cell-ref (fluid *packages*)))))

;; Check that the given package does not require a more recent version
;; of the installation library, and fail if it is the case.
(define (check-package pkg)
  (and-let* ((req (package-version-requirement pkg))
             ((or (not (= (version-major req)
                          (version-major install-lib-version)))
                  (> (version-minor req)
                     (version-minor install-lib-version)))))
            (display-error-and-exit
             "package "(package-name pkg)" needs a newer "
             "version of install-lib: "(version->string req)"\n"
             "(installed: " (version->string install-lib-version) ")")))

;; Like load-package but additionally check loaded packages.
(define (load&check-packages)
  (and-let* ((pkgs (load-packages))
             ((for-each check-package pkgs)))
            pkgs))

(define (load-package-in dir)
  (with-cwd dir (load-quietly package-definition-file)))

;;
;; Package options
;;

(define-record-type pkg-opt
  (really-make-pkg-opt key
                       help
                       arg-help
                       required-arg?
                       optional-arg?
                       default
                       parse
                       show
                       transform)
  pkg-opt?
  (key pkg-opt-key)
  (help pkg-opt-help)
  (arg-help pkg-opt-arg-help)
  (required-arg? pkg-opt-required-arg?)
  (optional-arg? pkg-opt-optional-arg?)
  (default pkg-opt-default)
  (parse pkg-opt-parse)
  (show pkg-opt-show)
  (transform pkg-opt-transform))

(define (make-pkg-opt key help arg-help req-arg? opt-arg? default . rest)
  (let-optionals rest ((parse identity)
                       (show identity)
                       (transform (lambda (old new) new)))
    (really-make-pkg-opt key
                         help
                         arg-help
                         req-arg?
                         opt-arg?
                         default
                         parse
                         show
                         transform)))

;; Return the name of PKG-OPT
(define (pkg-opt-name pkg-opt)
  (symbol->string (pkg-opt-key pkg-opt)))

;; Convert PKG-OPT into an SRFI-37 option.
(define (pkg-opt->option pkg-opt)
  (let ((key (pkg-opt-key pkg-opt))
        (transform (pkg-opt-transform pkg-opt))
        (parse (pkg-opt-parse pkg-opt)))
    (option (list (pkg-opt-name pkg-opt))
            (pkg-opt-required-arg? pkg-opt)
            (pkg-opt-optional-arg? pkg-opt)
            (lambda (opt name arg alist)
              (alist-replace key
                             (transform (alist-get key alist) (parse arg))
                             alist)))))

;; Return a pair (key, default) which associates the default value of
;; PKG-OPT to its key.
(define (pkg-opt-key&default pkg-opt)
  (cons (pkg-opt-key pkg-opt) (pkg-opt-default pkg-opt)))

;; Return the list of all package options of the PACKAGES.
(define (all-package-options packages)
  (append-map
   (lambda (pkg)
     (cond ((package-extension pkg 'options)
            => (lambda (opts)
                 (map (lambda (args) (apply make-pkg-opt args)) opts)))
           (else '())))
   packages))

;;
;; Load script handling
;;

(define load-script-name "load.scm")

;; Evaluate THUNK with CURRENT-OUTPUT-PORT opened on the current
;; package's loading script (in the install directory). During a dry
;; run, or when only non-shared data has to be installed, do nothing.
(define (with-output-to-load-script* thunk)
  (if (and (not (get-option-value 'dry-run)) (active-location? 'base))
      (let* ((dir (get-directory 'base #t))
             (file (absolute-file-name load-script-name dir)))
        (-create-directory&parents dir)
        (delete-file-or-fail file)
        (with-output-to-file file thunk))))

;; Sugar for with-output-to-load-script*.
(define-syntax with-output-to-load-script
  (syntax-rules ()
    ((with-output-to-load-script body ...)
     (with-output-to-load-script* (lambda () body ...)))))

;; Pretty-print all the elements of s-exps, one after the other, to
;; the current package's loading script (in the install directory).
(define (write-to-load-script s-exps)
  (with-output-to-load-script (for-each p s-exps)))

;;
;; Actions
;;

;; Perform all actions required to make the given version of the
;; package active (i.e. the default version for that package).
(define (activate-package layout pkg)
  (if (active-location? 'active)
      (let ((lnk-name (absolute-file-name (package-name pkg)
                                          (layout-dir layout 'active))))
        (if (and (file-exists? lnk-name) (file-symlink? lnk-name))
            (-delete-file lnk-name))
        (-create-symlink (relative-file-name (layout-dir layout 'base)
                                             (file-name-directory lnk-name))
                         lnk-name))))

(define (install-thing% layout name-or-pair location target-rel-dir perms)
  (let* ((target-dir (absolute-file-name target-rel-dir
                                         (layout-dir layout location)))
         (source (if (pair? name-or-pair) (car name-or-pair) name-or-pair))
         (target-name (file-name-nondirectory (if (pair? name-or-pair)
                                                  (cdr name-or-pair)
                                                  name-or-pair)))
         (target (absolute-file-name target-name target-dir)))
    (if (not ((get-option-value 'exclude) source))
        (begin
          (-create-directory&parents target-dir)
          (cond ((or (file-regular? source) (file-symlink? source))
                 (-copy-file source target))
                ((file-directory? source)
                 (if (file-exists? target)
                     (if (file-directory? target)
                         (if (get-option-value 'force)
                             (-set-file-mode (open-input-file target) (file:mode source))
                             (display-error-and-exit
                              "target directory already exists: " target))
                         (begin
                           (delete-file-or-fail target)
                           (-create-directory target (file:mode source))))
                     (-create-directory target (file:mode source)))
                 (install-directory-contents% layout
                                              source
                                              location
                                              (absolute-file-name
                                               target-name
                                               target-rel-dir)
                                              perms))
                (else (display-error-and-exit
                       "cannot install file-system object: " source)))))))

(define (install-directory-contents% layout
                                     name
                                     location
                                     target-rel-dir
                                     perms)
  (for-each (lambda (thing)
              (install-thing% layout thing location target-rel-dir perms))
            (map (lambda (f) (absolute-file-name f name))
                 (directory-files name #t))))

(define (install-thing name-or-pair location . rest)
  (if (active-location? location)
      (let-optionals rest ((target-rel-dir ".") (perms default-perms))
        (install-thing% (fluid *install-layout*)
                        name-or-pair
                        location
                        target-rel-dir
                        perms))))

(define (install-things names-or-pairs . rest)
  (for-each (lambda (name-or-pair)
              (apply install-thing name-or-pair rest))
            names-or-pairs))

(define install-file install-thing)
(define install-files install-things)
(define install-directory install-thing)
(define install-directories install-things)

(define (install-directory-contents name location . rest)
  (if (active-location? location)
      (let-optionals rest ((target-rel-dir ".") (perms default-perms))
        (install-directory-contents% (fluid *install-layout*)
                                     name
                                     location
                                     target-rel-dir
                                     perms))))

(define (install-string% layout str target-name location target-rel-dir perms)
  (let* ((target-dir (absolute-file-name target-rel-dir
                                         (layout-dir layout location)))
         (target-full-name (absolute-file-name target-name target-dir)))
    (-create-directory&parents target-dir)
    (delete-file-or-fail target-full-name)
    (-call-with-output-file target-full-name
      (lambda (port) (write str port)))
    (-set-file-mode (open-input-file target-full-name) (get-perms perms target-full-name))))

(define (install-string str target-name location . rest)
  (let-optionals rest ((target-rel-dir ".") (perms default-perms))
    (if (active-location? location)
        (install-string% (fluid *install-layout*)
                         str
                         target-name
                         location
                         target-rel-dir
                         perms))))

(define *layout* (make-fluid #f))
(define *install-layout* (make-fluid #f))

;; Return the directory identified by LOCATION in the current layout.
;; If INSTALL? is true, return the directory valid during the
;; installation of the package, otherwise return the directory valid
;; after installation (i.e. during package use).
(define (get-directory location install?)
  (layout-dir (fluid (if install? *install-layout* *layout*)) location))

;; Perform all actions to install PKG in INSTALL-LAYOUT. If LAYOUT is
;; not the same as INSTALL-LAYOUT, assume that some external tool will
;; move the installed files so that they are laid out according to
;; LAYOUT.
(define (install-package layout install-layout pkg)
  (with-cwd (package-directory pkg)
    (let-fluids *layout* layout
                *install-layout* install-layout
                (package-install-thunk pkg))))

;; Install all PACKAGES with the given OPTIONS-VALUES.
(define (install-packages packages options-values)
  (let* ((prefix (alist-get 'prefix options-values))
         (dest-dir (alist-get 'dest-dir options-values))
         (dest-prefix (and prefix (string-append dest-dir prefix)))
         (layout-fn (resolve-layout (alist-get 'layout options-values)))
         (activate? (not (alist-get 'inactive options-values))))
    (let-fluids *options-values* options-values
      (lambda ()
        (for-each
         (lambda (pkg)
           (let* ((rel-layout (layout-fn pkg))
                  (layout (absolute-layout rel-layout prefix))
                  (i-layout (absolute-layout rel-layout dest-prefix)))
             (install-package layout i-layout pkg)
             (if activate?
                 (activate-package i-layout pkg))))
         packages)))))

(define (install-sub-package dir . rest)
  (let-optionals rest ((options-diff '()))
    (with-cwd dir
      (install-packages
       (load&check-packages)
       (fold (lambda (diff options)
               (cond ((pair? diff)
                      (cons diff (alist-delete (car diff) options)))
                     ((symbol? diff)
                      (alist-delete diff options))
                     (else
                      (error "invalid option difference" diff))))
             (fluid *options-values*)
             options-diff)))))

;;
;; Error handling
;;

;; Display all the MSGS, then exit with an error code of 1.
(define (display-error-and-exit . msgs)
  (for-each display (cons "Error: " msgs))
  (newline)
  (exit 1))

;; Usage for all options. This also defines an order on options, and
;; this order is used when displaying available options to the user.
(define options-usage
  `(("help" #f "display this help message, then exit")
    ("version" #f "display version of install-lib, then exit")
    ("prefix" "dir" "specify directory where files are installed")
    ("layout" "layout" ("specify layout of installation directory"
                        ,(string-append "(predefined: "
                                        (string-join
                                         (map car predefined-layouts)
                                         ", ")
                                        ")")))
    ("dry-run" #f "don't do anything, print what would have been done")
    ("verbose" #f "print messages about what is being done")
    ("inactive" #f "don't activate package after installing it")
    ("non-shared-only" #f "only build/install platform-dependent files")
    ("force" #f "overwrite existing files during installation")
    ("no-user-defaults" #f "don't read defaults from ~/.scsh-pkg-defaults.scm")
    ;; advanced
    ("phases" "phases" ("perform only the given phase(s) "
                        ,(string-append "("all-phases-str")")))
    ("build" "name" "name of platform for which to build")
    ("layout-from" "file" "load layout of installation directory from file")
    ("dest-dir" "dir" ("specify prefix to use during installation"
                       "(use only for staged installations!)"))))

(define (sort-options options)
  (filter-map (lambda (name)
                (find (lambda (opt) (string=? (option-long-name opt) name))
                      options))
              (map first options-usage)))

;; Return the long name of the given OPTION. It is an error if that
;; option doesn't have a long name.
(define (option-long-name option)
  (first (filter string? (option-names option))))

;; Return the usage string for an option with OPT-NAME as long name,
;; and OPT-ARG as argument template. If OPT-ARG is #f, the option
;; doesn't take an argument.
(define (option/arg-usage opt-name opt-arg)
  (string-append "--" opt-name
                 (if opt-arg (string-append " <" opt-arg ">") "")))

;; Return the usage list for the given SRFI-37 OPTION.
(define (srfi-37-opt-usage option)
  (let ((raw-usage (assoc (option-long-name option) options-usage)))
    (cons (option/arg-usage (first raw-usage) (second raw-usage))
          (third raw-usage))))

;; Return the usage list for the given package OPTION.
(define (pkg-opt-usage option)
  (cons (option/arg-usage (pkg-opt-name option) (pkg-opt-arg-help option))
        (string-append
         (pkg-opt-help option)
         " [" ((pkg-opt-show option) (pkg-opt-default option)) "]")))

;; Return a complete usage string, considering the fact that the
;; script is called PROG-NAME and takes options BASE-OPTS (a list of
;; SRFI-37 options), and the package takes options PKG-OPTS (a list of
;; pkg-option records).
(define (build-usage-string prog-name base-opts pkg-opts)
  (let* ((base-usages (map srfi-37-opt-usage (sort-options base-opts)))
         (pkg-usages (map pkg-opt-usage pkg-opts))
         (max-arg-len (apply max (map (compose string-length car)
                                      (append base-usages pkg-usages)))))
    (with-current-output-port (make-string-output-port)
      (display-all "Usage: "prog-name" [options]\n\n"
                   "options:\n")
      (let ((format-usage
             (lambda (usage-pair)
               (let ((arg (car usage-pair)) (msg (cdr usage-pair)))
                 (display-all "  "
                              arg
                              (spaces (+ 1 (- max-arg-len
                                              (string-length arg))))
                              (if (list? msg) (first msg) msg)
                              #\newline)
                 (if (list? msg)
                     (for-each (lambda (line)
                                 (display-all (spaces (+ 3 max-arg-len))
                                              line
                                              #\newline))
                               (cdr msg)))))))
      (for-each format-usage base-usages)
      (if (not (null? pkg-usages))
          (begin
            (display "\npackage-specific options:\n")
            (for-each format-usage pkg-usages)))
      (string-output-port-output (current-output-port))))))

(define usage-string #f)

;; Display the usage string, then exit successfully.
(define (display-usage-and-exit)
  (display usage-string)
  (newline)
  (exit))

(define (display-version-and-exit)
  (display-all "scsh-install-lib version "
               (version->string install-lib-version)
               "\n")
  (exit))

;; Display a hint about how to use the just-installed PACKAGES,
;; assuming they were laid out according to LAYOUT and PREFIX.
(define (display-use-hint prefix layout packages skipped-pkgs)
  (let* ((active-locations (delete-duplicates
                            (map (lambda (pkg)
                                   (string-append "\""
                                                  (layout-dir (absolute-layout
                                                               (layout pkg)
                                                               prefix)
                                                              'active)
                                                  "\""))
                                 packages)
                            string=?))
         (load-scripts (map (lambda (pkg)
                              (absolute-file-name load-script-name
                                                  (package-name pkg)))
                            packages))
         (scsh-options (map (lambda (load-script)
                              (string-append "-lel " load-script))
                            load-scripts)))
    (if (not (null? packages))
        (begin
          (display
           (as-string
            "The following scsh package" (if (many? packages) "s were" " was")
            " installed successfully:\n"
            "  "(string-join (map package-full-name packages) ", ")"\n"))
          (if (not (every (lambda (script)
                            (find-library-file script (lib-dirs) ""))
                          load-scripts))
              (display
               (as-string
                "To make sure scsh finds "(if (many? packages) "them" "it")", "
                "please add the following value"
                (if (many? active-locations) "s" "")"\n"
                "to the environment variable SCSH_LIB_DIRS (quotes included):"
                "\n  " (string-join active-locations " ") "\n")))
          (display
           (as-string
            "To use "
            (if (many? packages) "these packages" "this package")
            " in a script, invoke scsh with the following option"
            (if (many? scsh-options) "s" "")":\n"
            "  "(string-join scsh-options " ")"\n"))))
    (if (not (null? skipped-pkgs))
        (display
         (as-string "The following scsh package"
                    (if (many? skipped-pkgs) "s were" " was")
                    " *not* installed, because "
                    (if (many? skipped-pkgs) "they were" "it was") "\n"
                    "already present (use --force option to force"
                    " reinstallation):\n"
                    "  "(string-join (map package-full-name skipped-pkgs) ", ")
                    "\n")))))

;;
;; Command line parsing
;;

;; Predefined parsers/unparsers
(define (parse-boolean s)
  (cond ((string=? s "yes") #t)
        ((string=? s "no") #f)
        (else (display-error-and-exit
               "unknown boolean value '"s"'. Use 'yes' or 'no'."))))

(define (show-boolean b)
  (if b "yes" "no"))

;; The identity function, sometimes useful for parsers/unparsers.
(define (identity x) x)

;; Fluid containing the value of all options.
(define *options-values* (make-fluid #f))

(define package-definition-file "pkg-def.scm")

(define (get-option-value key)
  (alist-get key (fluid *options-values*)))

(define (alist-updater key parser)
  (lambda (opt name arg alist)
    (alist-replace key (parser arg) alist)))

(define (alist-arg-updater key)
  (alist-updater key identity))

(define (alist-boolean-updater key)
  (alist-updater key (lambda (arg) (or (not arg) (parse-boolean arg)))))

(define common-options
  (list
   (option '(#\h "help") #f #f (lambda args (display-usage-and-exit)))
   (option '(#\v "version") #f #f (lambda args (display-version-and-exit)))
   (option '("prefix") #t #f (alist-arg-updater 'prefix))
   (option '("dest-dir") #t #f (alist-arg-updater 'dest-dir))
   (option '("layout") #t #f (alist-arg-updater 'layout))
   (option '("layout-from") #t #f (alist-updater 'layout load-layout))
   (option '("dry-run") #f #t (alist-boolean-updater 'dry-run))
   (option '("verbose") #f #t (alist-boolean-updater 'verbose))
   (option '("force") #f #t (alist-boolean-updater 'force))
   (option '("no-user-defaults") #f #f (lambda (opt name arg alist) alist))
   (option '("phases") #t #f (alist-updater 'phases parse-phases))))

(define lib-options
  (list
   (option '("build") #t #f (alist-arg-updater 'build))
   (option '("non-shared-only") #f #t
           (alist-boolean-updater 'non-shared-only))
   (option '("inactive") #f #t (alist-boolean-updater 'inactive))))

(define no-user-defaults-option "--no-user-defaults")

(define (options pkg-kind)
  (append (if (kind-lib? pkg-kind) lib-options '())
          common-options))

(define (parse-options args options defaults)
  (args-fold args
             options
             (lambda (option name . rest)
               (display-error-and-exit "Unknown option "name"\n"
                                       "(use --help to get a list of "
                                       "valid options)"))
             (lambda (operand . rest)
               (display-error-and-exit "Don't know what to do with "operand"\n"
                                       "(use --help to get a list of "
                                       "valid options)"))
             defaults))

;; Return user-specific defaults.
(define (read-user-defaults)
  (let ((file (expand-file-name "~/.scsh-pkg-defaults.scm")))
    (if (file-exists? file)
        (let ((defaults (load-single-sexp file)))
          (eval (list 'quasiquote defaults) (interaction-environment)))
        '())))

(define (kind-lib? pkg-kind)
  (cond ((eq? pkg-kind 'library) #t)
        ((eq? pkg-kind 'program) #f)
        (else (error "invalid package kind" pkg-kind))))

(define (options-defaults pkg-kind)
  `((prefix          . ,(if (kind-lib? pkg-kind) #f "/usr/local"))
    (dest-dir        . "")
    (layout          . ,(if (kind-lib? pkg-kind) "scsh" "fhs-program"))
    (build           . ,(host))
    (non-shared-only . #f)
    (inactive        . ,(not (kind-lib? pkg-kind)))
    (dry-run         . #f)
    (verbose         . #f)
    (force           . #f)
    (phases          . ,all-phases)
    (exclude         . ,(lambda args #f))))

;; Partition PACKAGES in two sets: the ones which are not installed
;; yet, and the ones which are already. If FORCE? is true, pretend
;; that all packages are not installed yet.
(define (partition-packages prefix layout packages force? non-shared-only?)
  (partition (lambda (pkg)
               (or force?
                   (let ((abs-layout (absolute-layout (layout pkg) prefix)))
                     (file-not-exists?
                      (if non-shared-only?
                          (paths->file-name (layout-dir abs-layout 'lib) (host))
                          (layout-dir abs-layout 'base))))))
             packages))

(define (install-main-internal cmd-line display-hint?)
  (let* ((prog (file-name-nondirectory (car cmd-line)))
         (args (cdr cmd-line))
         (packages (load&check-packages))
         (all-pkg-opts (all-package-options packages)))
    (set! usage-string (build-usage-string prog
                                           (options 'library)
                                           all-pkg-opts))
    (let* ((all-opts (append (options 'library)
                             (map pkg-opt->option all-pkg-opts)))
           (all-dfts (append (alist-combine
                              (if (member no-user-defaults-option args)
                                  '()
                                  (read-user-defaults))
                              (options-defaults 'library))
                             (map pkg-opt-key&default all-pkg-opts)))
           (options-values (parse-options args all-opts all-dfts))
           (phases (alist-get 'phases options-values))
           (prefix (alist-get 'prefix options-values))
           (layout (alist-get 'layout options-values))
           (force? (alist-get 'force options-values))
           (non-shared-only? (alist-get 'non-shared-only options-values)))
      (if (null? packages)
          (display-error-and-exit
           "no package to install"
           (if (file-not-exists? package-definition-file)
               (as-string "\n(cannot find file called \""
                          package-definition-file
                          "\" in current directory)")
               "")))
      (if (not prefix)
          (display-error-and-exit "no prefix specified (use --prefix option)"))
      (if (not (file-name-absolute? prefix))
          (display-error-and-exit "prefix must be an absolute path"))
      (let ((resolved-layout (resolve-layout layout)))
        (if (not resolved-layout)
            (display-error-and-exit "invalid layout "layout))
        (if (and (not (lset= eq? phases all-phases))
                 (any (lambda (req) (version<? req '(1 2 0)))
                      (filter-map package-version-requirement packages)))
            (display-error-and-exit
             "at least one of the packages about to be installed might not\n"
             "understand the --phases option. Re-run without using --phases."))
        (receive (pkgs-install pkgs-skip)
                 (partition-packages prefix
                                     resolved-layout
                                     packages
                                     force?
                                     non-shared-only?)
                 (install-packages pkgs-install options-values)
                 (if (and display-hint? (member 'install phases))
                     (display-use-hint prefix
                                       resolved-layout
                                       pkgs-install
                                       pkgs-skip)))))))

(define (install-main-quiet cmd-line)
  (install-main-internal cmd-line #f))

(define (install-main cmd-line)
  (install-main-internal cmd-line #t))

(define (install-program-main cmd-line)
  (let* ((prog (file-name-nondirectory (car cmd-line)))
         (args (cdr cmd-line))
         (packages (cell-ref (fluid *packages*)))
         (all-pkg-opts (all-package-options packages)))
    (set! usage-string (build-usage-string prog
                                           (options 'program)
                                           all-pkg-opts))
    (for-each check-package packages)
    (let* ((all-opts (append (options 'program)
                             (map pkg-opt->option all-pkg-opts)))
           (all-dfts (append (alist-combine
                              (if (member no-user-defaults-option args)
                                  '()
                                  (read-user-defaults))
                              (options-defaults 'program))
                             (map pkg-opt-key&default all-pkg-opts)))
           (options-values (parse-options args all-opts all-dfts))
           (prefix (alist-get 'prefix options-values))
           (layout (alist-get 'layout options-values))
           (force? (alist-get 'force options-values)))
      (if (null? packages)
          (display-error-and-exit
           "no package to install"))
      (if (not (file-name-absolute? prefix))
          (display-error-and-exit "prefix must be an absolute path"))
      (let ((resolved-layout (resolve-layout layout)))
        (if (not resolved-layout)
            (display-error-and-exit "invalid layout "layout))
        (install-packages packages options-values)))))
