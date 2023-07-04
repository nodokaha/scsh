;;; Copyright (c) 2012 Johan Ceuppens                                           
;;;                                                                             
;;; All rights reserved.                                                        
;;;                                                                             
;;; Redistribution and use in source and binary forms, with or without          
;;; modification, are permitted provided that the following conditions          
;;; are met:                                                                    
;;; 1. Redistributions of source code must retain the above copyright           
;;;    notice, this list of conditions and the following disclaimer.            
;;; 2. Redistributions in binary form must reproduce the above copyright        
;;;    notice, this list of conditions and the following disclaimer in the      
;;;    documentation and/or other materials provided with the distribution.     
;;; 3. The name of the authors may not be used to endorse or promote products   
;;;    derived from this software without specific prior written permission.    
;;;                                                                             
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR       
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES   
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.     
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,           
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT    
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY       
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT         
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF    
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

;; Copyright (C) Johan Ceuppens 2013
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed chunk size gc, can be used in meta-gc.scm API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-sized-fast-gc)
  (let ((*heap '())
	(*null '())
	(*max-datasize 1024)	
	(*datasize 0)	
	(*currentn 0)	
	(*max-chunksize 8)	
	(*dynamic-data '())	
	)
    
    (define (malloc)
      (add-to-heap! 1)
      )

    (define (calloc nchunks)
      (add-to-heap! nchunks)
      )

    ;; private procedures
    
    (define (get-heap) *heap)
    
    ;; data is a string
    (define (get-data-size d) (string-length d))
    
    (define (generate-error msg)
      (cond ((eq? msg 'memory-exhausted)
	     (display "memory exhausted")(newline))
	    ))
    
    (define (add-to-heap! n)
      (do ((n2 0 (+ n2 1)))
	  ((>= n2 n) *heap)
	(set! *heap (append (get-heap) (list (make-chunk)))))
      )
    
    (define (make-chunk)
      (cons 'chunk *null))  
    
    ;;NOTE append null string after <8
    ;; data is a string
    (define (split-data data)
      (let ((s ""))
	(let ((retl '()))
	  (do ((n2 0 (+ n2 1)))
	      ((>= n2 (string-length data))
	       retl)
	    (do ((n 0 (+ n 1)))
		((or (>= n 8)(>= (+ n2 n) (string-length data)))
		 (append retl (list s))(set! n2 (+ n2 n)))
	      (string-append s (string-ref data n))
	      ))
	  )))
  
    ;; FIX over *currentn
    (define (set-data-rec! lst n)
      (cond ((eq? n 0)
	     '())
	    ;; reverses
	    (else (append (list (list-ref lst n))
			  (set-data-rec! lst (- n 1))))))
				

    (define (set-data! chunk data)
      (let ((lst (split-data data)))
	(set-data-rec! lst (length lst))
	(set! *currentn (+ n *currentn))
	))
    
    (define (set-chunk-data! chunk)
      (set! *datasize (+ *datasize (get-data-size chunk)))
      (if (> *datasize *max-datasize) 
	  (generate-error 'memory-exhausted)
	  ;; The data is dynamically bound in this actor
	  (set-data! chunk *dynamic-data)
	  ))
    
    (define (dispatch msg)
      (cond ((eq? msg 'malloc) malloc)	
	    ((eq? msg 'calloc) calloc)
	    (else (display "make-gc : message not understood : ")(display msg)(newline))
	    ))
    dispatch))		
