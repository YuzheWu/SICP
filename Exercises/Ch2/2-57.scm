;;; SICP Exercise 2.57 by Yuzhe Wu, 7 Nov 2019


;; modify representation of sums
(define (make-sum-aux a1 a2) 
  (cond ((=number? a1 0) a2) 
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) 
	 (+ a1 a2))
	((and (sum? a1) (not (sum? a2)))
	 (append a1 (list a2)))
	((and (not (sum? a1)) (sum? a2))
	 (append '(+) (list a1) (cdr a2)))
	((and (sum? a1) (sum? a2))
	 (append a1 (cdr a2)))
	(else (list '+ a1 a2))))

(define (make-sum . args)
  (if (= (length args) 1)
      (car args)
      (make-sum-aux (car args)
		    (apply make-sum (cdr args)))))


(define (addend s) (cadr s))
(define (augend s) (apply make-sum (cddr s)))


;; modify representation of products
(define (make-product-aux m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	((and (product? m1) (not (product? m2)))
	 (append m1 (list m2)))
	((and (not (product? m1)) (product? m2))
	 (append '(*) (list m1) (cdr m2)))
	((and (product? m1) (product? m2))
	 (append m1 (cdr m2)))
	(else (list '* m1 m2))))

(define (make-product . args)
  (if (= (length args) 1)
      (car args)
      (make-product-aux (car args)
			(apply make-product (cdr args)))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (apply make-product (cddr p)))
