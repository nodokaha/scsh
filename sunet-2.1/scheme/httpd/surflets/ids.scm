(define (surflet-file-name req)
  (last (http-url-path (surflet-request-url req))))

;; This works for all requests except for the initial one. For the
;; initial one (main's arg) think about using instance-session-id.
(define (my-session-id req)
  (resume-url-session-id (surflet-file-name req)))

;;  This works for all requests except for the initial one: we don't
;;  have a continuation at this time.
(define (my-continuation-id req)
  (resume-url-continuation-id (surflet-file-name req)))

;; Returns two values: session-id and continuation-id. The
;; restrictions from my-session-id and my-continuation-id apply here
;; as well.
(define (my-ids req)
  (resume-url-ids (surflet-file-name req)))

