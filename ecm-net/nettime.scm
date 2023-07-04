;;; nettime.scm -- obtain the time on remote machines
;;
;; $Id: nettime.scm,v 1.1 1998/04/29 06:43:32 ecm Exp $
;;
;;     Copyright (C) 1998  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>



;;; Overview ========================================================
;;
;; Most Unix hosts provide a Daytime service which sends the current
;; date and time as a human-readable character string. The daytime
;; service is typically served on port 13 as both TCP and UDP.
;;
;; The Time protocol provides a site-independent, machine readable
;; date and time. A "time" consists of the number of seconds since
;; midnight on 1st January 1900. The Time service is typically served
;; on port 37 as TCP and UDP. The idea is that you can confirm your
;; system's idea of the time by polling several independent sites on
;; the network.


;;; Related work ======================================================
;;
;; * Time.pm is a Perl module by Graham Barr
;; * rfc868 describes the Time protocol
;; * rfc867 describes the Daytime protocol in all its glory
;; * for a genuinely useful protocol look at the Network Time Protocol
;; defined in rfc1305, which allows for the synchronization of clocks
;; on networked computers.



;; args host protocol, where host may be an IP number or a fqdn. we
;; subtract 70 years' worth of seconds at the end, since the time
;; protocol returns the number of seconds since 1900, whereas Unix
;; time is since 1970.
(define (net:time host tcp/udp)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "time" "tcp"))         
         (sock (socket-connect protocol-family/internet
                               tcp/udp
                               (host-info:name hst-info)
                               (service-info:port srvc-info)))
         (result (read-integer (socket:inport sock))))
    (close-socket sock)
    (- result 2208988800)))


(define (net:daytime host tcp/udp)
  (let* ((hst-info (host-info host))
         (srvc-info (service-info "daytime" "tcp"))
         (sock (socket-connect protocol-family/internet
                               tcp/udp
                               (host-info:name hst-info)
                               (service-info:port srvc-info)))
         (result (read-string 20 (socket:inport sock))))
    (close-socket sock)
    result))


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


;; EOF
