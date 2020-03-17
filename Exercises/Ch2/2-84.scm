;;; SICP Exercise 2.84 by Yuzhe Wu, 14 Mar 2020


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags))) 
      (if proc
	  (apply proc (map contents args)) 
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags)) 
		    (type2 (cadr type-tags)) 
		    (a1 (car args))
		    (a2 (cadr args))
		    (a1-raised (raise-to type2 a1))
		    (a2-raised (raise-to type1 a2)))
		(cond ((eq? type1 type2)
		       (error "No method for these types"
			      (list op type-tags)))
		      (a1-raised
		       (apply-generic op a1-raised a2))
		      (a2-raised
		       (apply-generic op a1 a2-raised))
		      (else (error "No method for these types"
				   (list op type-tags)))))
	      (error "No method for these types"
                     (list op type-tags)))))))

;; procedure for supertype coercion
(define (raise-to type obj)
  (let ((type-obj (type-tag obj))
	(arg-obj (contents obj)))
    (cond ((eq? type type-obj) obj)
	  ((get 'raise (list type-obj))
	   (raise-to type
		     ((get 'raise (list type-obj)) arg-obj)))
	  (else #f)))))
