(define-condition-type 'failure '(error))

(define failure? (condition-predicate 'failure))

(define (fail msg . irritants)
  (apply signal 'failure msg irritants))

(define (with-failure-handler* handler thunk)
  (with-handler
    (lambda (condition decline)
      (if (failure? condition)
	  (handler (condition-stuff condition))
          (decline))
      thunk)))


(define-syntax assert
  (syntax-rules ()
    ((assert ?x ?y0 ...)
     (if (not ?x) (fail "Assertion failed" '?x ?y0 ...))) ))

(define-syntax deny
  (syntax-rules ()
    ((deny ?x ?y0 ...)
     (assert (not ?x) ?y0 ...))))

(define (should-raise* condition-predicate? thunk)
  (with-handler (lambda (condition decline)
                  (assert (condition-predicate? condition)))
                thunk))

(define (shouldnt-raise* condition-predicate? thunk)
  (with-handler (lambda (condition decline)
                  (deny (condition-predicate? condition))
                  (decline))
                thunk))

(define-syntax should-raise
  (syntax-rules ()
    ((should-raise condition-predicate? e0 e1 ...)
     (should-raise* condition-predicate?
                    (lambda () e0 e1 ...)))))

(define-syntax shouldnt-raise
  (syntax-rules ()
    ((shouldnt-raise condition-predicate? e0 e1 ...)
     (shouldnt-raise* condition-predicate?
                      (lambda () e0 e1 ...)))))

(define-record-type :testcase
  (make-testcase description thunk)
  testcase?
  (description testcase-description)
  (thunk testcase-thunk))
