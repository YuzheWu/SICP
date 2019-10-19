;;; SICP Exercise 2.56 by Yuzhe Wu, 20 Oct 2019


;; update the 'deriv' program
(define (deriv exp var) 
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0)) 
	((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
	  (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (make-exponentiation (base exp)
							  (- (exponent exp) 1)))
		       (deriv (base exp) var)))
	(else
	 (error "unknown expression type: DERIV" exp))))

;; define predicate, selectors and constructors for exponentiation
(define (exponentiation? x) (and (pair? x) (eq? (car x) '^)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((= exponent 1) base)
	((= exponent 0) 1)
	(else (list '^ base exponent))))



