;;Copyright (c) 2015, Johan Ceuppens (goon)
;;All rights reserved.

;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

;;1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

;;2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

;;3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; <---

(load "init.scm")
(load "util.scm")
(load "server.scm")
(load "db.scm")

(define (make-server-dns)

  (define (loopfunc in out)
	(let ((*server (make-server loopfunc))
		(*db (make-db)))	
	(let ((dns-question (string->byte-vector (read in)))i
	      (dns-answer #f)
	      (name #f)
		(let ((n (byte-vector-ref dns-question 1)))
		  	(if (= n 11)
			  	(let ((m 0))
		  		(do ((i n+2 (+ i 1))
			     		((= i (byte-vector-length dns-question)) (display "received domain name lookup")))
				  (if (= (bytevector-ref i) 0)
				    (set! m (+ m 1))
				    (if (>= m 7)
				      (display "EOT")))
				  (set! name (string-append name (byte-vector-ref i)))
				  (set! dns-answer ((*db 'lookup) name))
				  )
				)))
		)
		

    	(define (dispatch msg)
      		(cond ((eq? msg 'start) (*server 'start))
	    		((eq? msg 'stop) (*server 'stop))
	    		((eq? msg 'restart) (*server 'restart))
			((eq? msg 'add) (*db 'add)) 
			((eq? msg 'lookup) (*db 'lookup) ) 
	    		(else (display "make-server-dns : message not understood."))))

    	dispatch)))
