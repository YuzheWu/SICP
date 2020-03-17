;;; SICP Exercise 2.89 by Yuzhe Wu, 16 Mar 2020


(define (adjoin-term term term-list)
  (cond ((=zero? (coeff term))
	 term-list)
	((empty-termlist? term-list)
	 (if (= (order term) 0)
	     (list (coeff term))
	     (adjoin-term term (list 0))))
	((= (order term) 
	    (1+ (order (first-term term-list))))
	 (cons (coeff term) term-list))
	(else
	 (adjoin-term term (cons 0 term-list)))))
(define (the-empty-term-list) '())
(define (first-term term-list) 
  (list (-1+ (length term-list))
	(car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term)) 
(define (coeff term) (cadr term))

