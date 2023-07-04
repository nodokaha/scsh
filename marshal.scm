#! /usr/local/bin/scsh \
-o dump/restore -e timings -s
!#

;; Author: Eric Marsden  <emarsden@laas.fr>
;; Time-stamp: <2000-07-06 emarsden>
;;
;; An example application of the dump/restore code in vm/dump.scm in
;; the scsh distribution. Generates an ASCII encoding of an arbitrary
;; data structure, which can be saved to a file and later restored.
;; Unfotunately this is no faster than using `read' and `write'.

(define (marshal object stream)
  (dump object (lambda (c) (write-char c stream)) -1))

(define (unmarshal stream)
  (restore (lambda () (read-char stream))))


;; generate some sample data 
(define *file-registry* '())

(define (register-directory dir)
  (for-each
   (lambda (entry)
     (with-cwd dir
        (cond ((file-directory? entry)
               (if (file-readable? entry)
                   (register-directory (string-append (cwd) "/" entry))))
              (else (register-file (string-append (cwd) "/" entry))))))
   (directory-files dir)))

(define (register-file file)
  (let ((finfo (file-info file)))
    (set! *file-registry*
          (cons (vector file
                        (file-info:mtime finfo)
                        (file-info:inode finfo)
                        (md5sum file))
                *file-registry*))))

(define (md5sum file)
  (if (and (file-regular? file)
           (file-readable? file))
      (car ((infix-splitter (rx white)) (car (run/strings (md5sum ,file)))))
      ""))


;; timing information comparing write+read and marshal+unmarshal
(define (encode writer reader)
  (let* ((filename (create-temp-file "/var/tmp/encoded"))
         (output (open-output-file filename))
         (before (time))
         (input #f))
    (writer *file-registry* output)
    (close output)
    (set! input (open-input-file filename))
    (reader input)
    (close input)
    (- (time) before)))

(define (timings argv)
  (register-directory (second argv))
  (format #t "With marshaling takes ~d seconds~%" (encode marshal unmarshal))
  (format #t "With write takes ~d seconds~%" (encode write read)))

;; EOF
