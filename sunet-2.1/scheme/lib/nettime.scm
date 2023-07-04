;;; nettime.scm -- obtain the time on remote machines

;;; This file is part of the Scheme Untergrund Networking package.

;;; Copyright (c) 1998 by Eric Marsden
;;; Copyright (c) 2003 by Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;; Related work ======================================================
;;
;; * Time.pm is a Perl module by Graham Barr
;; * rfc868 describes the Time protocol
;;   http://www.ietf.org/rfc/rfc868.txt
;; * rfc867 describes the Daytime protocol in all its glory
;;   http://www.ietf.org/rfc/rfc867.txt
;; * for a genuinely useful protocol look at the Network Time Protocol
;; defined in rfc1305, which allows for the synchronization of clocks
;; on networked computers.

;; args host protocol, where host may be an IP number or a fqdn. we
;; subtract 70 years' worth of seconds at the end, since the time
;; protocol returns the number of seconds since 1900, whereas Unix
;; time is since 1970.

(define (rfc868-time/tcp host)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "time" "tcp"))         
         (sock (socket-connect protocol-family/internet
                               socket-type/stream
                               (host-info:name hst-info)
                               (service-info:port srvc-info)))
         (result (read-integer (socket:inport sock))))
    (close-socket sock)
    (- result 2208988800)))

(define (rfc868-time/udp host . maybe-timeout)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "time" "udp"))
	 (timeout (if (pair? maybe-timeout)
		      (car maybe-timeout)
		      #f))
	 (socket (create-socket protocol-family/internet socket-type/datagram)))
    (connect-socket socket
		    (internet-address->socket-address
		     (car (host-info:addresses hst-info))
		     (service-info:port srvc-info)))
    (send-message socket "")
    (if (null? (select-ports timeout (socket:inport socket)))
	(begin
	  (close-socket socket)
	  #f)
	(with-fatal-error-handler*
	 (lambda (result punt)
	   ;; we may see a "connection refused" error here
	   #f)
	 (lambda ()
	   (let ((result (read-integer (socket:inport socket))))
	     (close-socket socket)
	     (- result 2208988800)))))))

(define (rfc867-daytime/tcp host)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "daytime" "tcp"))
         (sock (socket-connect protocol-family/internet
                               socket-type/stream
                               (host-info:name hst-info)
                               (service-info:port srvc-info)))
         (result (read-string 20 (socket:inport sock))))
    (close-socket sock)
    result))

(define (rfc867-daytime/udp host . maybe-timeout)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "daytime" "udp"))
	 (timeout (if (pair? maybe-timeout)
		      (car maybe-timeout)
		      #f))
	 (socket (create-socket protocol-family/internet socket-type/datagram)))
    (connect-socket socket
		    (internet-address->socket-address
		     (car (host-info:addresses hst-info))
		     (service-info:port srvc-info)))
    (send-message socket "")
    (if (null? (select-ports timeout (socket:inport socket)))
	(begin
	  (close-socket socket)
	  #f)
	(with-fatal-error-handler*
	 (lambda (result punt)
	   ;; we may see a "connection refused" error here
	   #f)
	 (lambda ()
	   (call-with-values
	    (lambda () (receive-message socket 20))
	    (lambda (result socket-address)
	      (close-socket socket)
	      result)))))))

;; read 4 bytes from fd and build an integer from them
(define (read-integer fd)
  (let loop ((accum 0)
             (remaining 4))
    (if (zero? remaining)
        accum
        (loop (+ (arithmetic-shift accum 8) (read-byte fd))
              (- remaining 1)))))

;; what about EOF??
(define (read-byte fd)
  (char->ascii (read-char fd)))
