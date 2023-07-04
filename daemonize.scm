;; a daemon does a chroot() to /, redirects its stdin and stdout from
;; /dev/null, and does a fork() and setsid(). 
(define (daemonize* thunk)
  (chdir "/")
  (close (current-input-port))
  (set-current-input-port! (open-input-file "/dev/null"))
  (close (current-output-port))
  (set-current-output-port! (open-output-file "/dev/null"))
  (if (fork) (exit))
  (become-session-leader)
  (close (error-output-port))
  (set-error-output-port! (open-output-file "/dev/null"))
  (thunk))            

(define-syntax daemonize
  (syntax-rules ()
    ((daemonize body ...)
     (daemonize* (lambda () body ...)))))
