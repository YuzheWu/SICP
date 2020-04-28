;;; SICP Exercise 3.19 by Yuzhe Wu, 28 Apr 2020


(define (has-cycle? x)
  (define (aux a b)
    (cond ((eq? a b) #t)
	  ((null? b) #f)
	  ((null? (cdr b)) #f)
	  (else (aux (cdr a) (cddr b)))))
  (aux x x))