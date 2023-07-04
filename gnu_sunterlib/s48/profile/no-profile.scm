(define (profile-init!)
  #f)

(define (display-profile)
  (display "No profiling data available.")
  (newline))

(define-syntax define-prof
  (syntax-rules
      ()
    ((_ ?args ...)
     (define ?args ...))))

(define-syntax account-for
  (syntax-rules
      ()
    ((_ ?account ?body ...)
     (begin
       ?body
       ...))))