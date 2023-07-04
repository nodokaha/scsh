;; poor man's test suite
;; run stuff  ,in onebol-testbed

(assert #t)

(deny #f)

(should-raise* failure? (lambda () (fail "")))

(should-raise* failure? (lambda () (assert #f)))

(should-raise* failure? (lambda () (deny #t)))

(should-raise* (lambda (condition)
                 (and (error? condition)
                      (not (failure? condition))))
               (lambda ()
                 (shouldnt-raise* failure? (lambda () (error "")))))
