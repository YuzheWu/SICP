;;; SICP Exercise 3.18 by Yuzhe Wu, 28 Apr 2020


;; We again use the 'set' data structure from Section 2.3.3 to keep track of 
;; cdrs that have already been visited.
(define (has-cycle? x)
  (let ((visited '()))
    (define (aux x)
      (cond ((null? x) #f)	  
	    ((element-of-set? x visited) #t)
	    (else (begin (set! visited (adjoin-set x visited))
			 (aux (cdr x))))))
    (aux x)))
	     