;;; SICP Exercise 2.11 by Yuzhe Wu, 9 Dec 2017

;;; In passing, Ben also cryptically comments: "By testing the signs of
;;; the endpoints of the intervals, it is possible to break mul-interval
;;; into nine cases, only one of which requires more than two multipl-
;;; ications." Rewrite this procedure using Ben's suggestion.



(define (mul-interval x y)
  (define (contain-zero? z)
    (and (not (positive? (lower-bound z)))
	 (not (negative? (upper-bound z)))))
  (let ((lower-x (lower-bound x))
	(upper-x (upper-bound x))
	(lower-y (lower-bound y))
	(upper-y (upper-bound y)))
    (cond ((and (positive? lower-x)
		(positive? lower-y))
	   (make-interval (* lower-x lower-y) (* upper-x upper-y)))
	  ((and (negative? upper-x)
		(negative? upper-y))
	   (make-interval (* upper-x upper-y) (* lower-x lower-y)))
	  ((and (positive? lower-x)
		(negative? upper-y))
	   (make-interval (* lower-y upper-x) (* lower-x upper-y)))
	  ((and (negative? upper-x)
		(positive? lower-y))
	   (make-interval (* lower-x upper-y) (* lower-y upper-x)))
	  ((and (contain-zero? x)
		(positive? lower-y))
	   (make-interval (* lower-x upper-y) (* upper-x upper-y)))
	  ((and (contain-zero? x)
		(negative? upper-y))
	   (make-interval (* upper-x lower-y) (* lower-x lower-y)))
	  ((and (contain-zero? y)
		(positive? lower-x))
	   (make-interval (* lower-y upper-x) (* upper-y upper-x)))
	  ((and (contain-zero? y)
		(negative? upper-x))
	   (make-interval (* upper-y lower-x) (* lower-y lower-x)))
	  (else
	   (let ((p1 (* lower-x upper-y))
		 (p2 (* lower-y upper-x))
		 (p3 (* lower-x lower-y))
		 (p4 (* upper-x upper-y)))
	     (make-interval (min p1 p2) (max p3 p4)))))))


;;; ====== TEST ====== 
;; constructor and selectors
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;; print procedure
(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]"))

;; test cases
(define x (make-interval -1 2))
(define y (make-interval 2 4))
(define z (make-interval -5 -4))

(print-interval (mul-interval x x)) ; -> [-2,4]
(print-interval (mul-interval x y)) ; -> [-4,8]
(print-interval (mul-interval x z)) ; -> [-10,5]
(print-interval (mul-interval y x)) ; -> [-4,8]
(print-interval (mul-interval y y)) ; -> [4,16]
(print-interval (mul-interval y z)) ; -> [-20,-8]
(print-interval (mul-interval z x)) ; -> [-10,5]
(print-interval (mul-interval z y)) ; -> [-20,-8]
(print-interval (mul-interval z z)) ; -> [16,25]