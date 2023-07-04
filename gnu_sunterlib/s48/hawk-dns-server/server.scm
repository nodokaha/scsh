
;;Copyright (c) 2015, Johan Ceuppens (goon)
;;All rights reserved.

;;Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

;;1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

;;2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

;;3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

;;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(define (make-server loopfunc)
	(let ((*socket #f)
	      (*in #f)
	      (*out #f)
	      (*lastport #f)
	      (*loopfunc loopfunc))

	  	(define (open port)
		  (set! *lastport port)
		  (set! *socket (open-socket port))) 

		(define (start port)
		  (open port)
		  (let loop ((s #f))
		    	(lambda ()
			  	(socket-accept *socket))
			(lambda (in out)
			  	  (set! *in in)
				  (set! *out out)
				  (*loopfunc in out)
				  )))

		(define (stop)
			(close-input-port *in)
			(close-output-port *out)
			)

		(define (restart)
		  	(stop)
			(if *lastport
				(start *lastport)
				(display "Could not restart - use start")))

		(define (dispatch msg)
			(cond ((eq? msg 'start) start)
				((eq? msg 'stop) stop)
				((eq? msg 'restart) restart)
				(else (display "make-server : message not understood"))))

		dispatch))
