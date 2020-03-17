;;; SICP Exercise 2.97 by Yuzhe Wu, 17 Mar 2020


(define (reduce-terms n d)
  (let ((a (gcd-terms n d)))
    (let ((c (coeff (first-term a)))
	  (O1 (max (order (first-term n))
		   (order (first-term d))))
	  (O2 (order (first-term a))))
      (let ((factor 
	     (list (make-term 
		    0 
		    (expt c (1+ (- O1 O2)))))))
	(let ((nn (car (div-terms 
			(mul-terms n factor)
			a)))
	      (dd (car (div-terms
			(mul-terms d factor)
			a))))
	  (let ((x (apply gcd (append (map coeff nn)
				      (map coeff dd)))))
	    (list (mul-terms nn (list (make-term 0 (/ 1 x))))
		  (mul-terms dd (list (make-term 0 (/ 1 x)))))))))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map (lambda (L) (make-poly (variable p1) L))
	   (reduce-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: REDUCE-POLY" (list p1 p2))))


