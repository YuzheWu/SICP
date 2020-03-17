;;; SICP Exercise 2.94 by Yuzhe Wu, 17 Mar 2020


;; to be included in the polynomial package
(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))
(define (gcd-terms a b) 
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (gcd-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: GCD-POLY" (list p1 p2))))
(put 'gcd '(polynomial polynomial) 
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

;; to be included in the Scheme-number package
(put 'gcd '(scheme-number scheme-number)
     gcd)

;; generic gcd procedure
(define (greatest-common-divisor x y)
  (apply-generic 'gcd x y))
