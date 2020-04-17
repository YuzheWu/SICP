;;; SICP Exercise 3.1 by Yuzhe Wu, 18 Apr 2020


(define (make-accumulator sum)
  (lambda (val)
    (begin (set! sum (+ sum val))
	   sum)))