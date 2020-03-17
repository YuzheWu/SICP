;;; SICP Exercise 2.96 by Yuzhe Wu, 17 Mar 2020


(define (psudoremainder-terms L1 L2)
  (let ((O1 (if (empty-termlist? L1)
		0
		(order (first-term L1))))
	(O2 (order (first-term L2)))
	(c (coeff (first-term L2))))
    (let ((factor (expt c (1+ (- O1 O2)))))
      (cadr (div-terms (mul-terms 
			L1
			(list (make-term 0 factor)))
		       L2)))))
(define (gcd-terms a b) 
  (define (simplify L)
    (let ((x (apply gcd (map coeff L))))
      (mul-terms L
		 (list (make-term 0 (/ 1 x))))))
  (define (gcd-terms-aux a b)
    (if (empty-termlist? b)
	a
	(gcd-terms-aux b (psudoremainder-terms a b))))
  (simplify (gcd-terms-aux a b)))
