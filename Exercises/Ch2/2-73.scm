;;; SICP Exercise 2.73 by Yuzhe Wu, 29 Feb 2020


;; part a)

;; The new deriv procedure applies the data-directed approach to the case where
;; the input expression is of a compound type, i.e. is neither a number nor a 
;; variable. It first extracts the operator type (+ or *) from the compound 
;; expression and then looks up the operation-and-type table for the applicable
;; deriv procedure. Since the operator and operands selectors used in the data-
;; directed dispatch assume a list structure, we need to treat single numbers 
;; and variables as special cases.


;; part b)

(define (install-deriv-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  (define (deriv-product operands var)
    (make-sum (make-product
	       (multiplier operands)
	       (deriv (multiplicand operands) var))
	      (make-product
	       (deriv (multiplier operands) var)
	       (multiplicand operands))))
  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-product)
  'done)


;; part d)

;; No change is needed.



