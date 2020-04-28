;;; SICP Exercise 3.8 by Yuzhe Wu, 28 Apr 2020


;; We use the data structure 'set' from Section 2.3.3 to keep track of the pairs
;; that have already been counted.
(define (count-pairs x)
  (let ((counted '()))
    (define (aux x)
      (if (or (not (pair? x))
	      (element-of-set? x counted))
	  0
	  (begin (set! counted (adjoin-set x counted))
		 (+ (aux (car x))
		    (aux (cdr x))
		    1))))
    (aux x)))

;; set operations from Section 2.3.3
(define (element-of-set? x set) 
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
  (if (element-of-set? x set)
      set
      (cons x set)))
  