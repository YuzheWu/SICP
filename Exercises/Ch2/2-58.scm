;;; SICP Exercise 2.58 by Yuzhe Wu, 26 Feb 2020


;; Part a)

;; modify representation of sums
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2))
	 (+ a1 a2))
	(else (list a1 '+ a2))))

(define (addend s) (car s))
(define (augend s) (caddr s))


;; modify representation of products
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))


;; Part b)

;; predicates
(define (get-operators expr)
  (if (< (length expr) 2)
      '()
      (append (list (cadr expr))
	      (get-operators (cddr expr)))))

(define (sum? expr)
  (if (memq '+ (get-operators expr))
      #t
      #f))

(define (only-products? operators)
  (if (null? operators)
      #t
      (and (eq? (car operators) '*)
	   (only-products? (cdr operators)))))

(define (product? expr)
  (only-products? (get-operators expr)))


;; selectors
(define (addend s)
  (define (aux result remainder)
    (if (eq? (car remainder) '+)
	result
	(aux (append result (list (car remainder))) (cdr remainder))))
  (strip (aux '() s)))

(define (augend s)
  (strip (cdr (memq '+ s))))

(define (multiplier p) (car p))
(define (multiplicand p) (strip (cddr p)))

(define (strip lst)
  (if (= (length lst) 1)
      (car lst)
      lst))


;; constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (let ((b1 (if (pair? a1) a1 (list a1)))
		    (b2 (if (pair? a2) a2 (list a2))))
		(append b1 '(+) b2)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (let ((n1 (if (and (pair? m1) (not (sum? m1))) m1 (list m1)))
		    (n2 (if (and (pair? m2) (not (sum? m2))) m2 (list m2))))
		(append n1 '(*) n2)))))
