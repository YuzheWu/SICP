;;; SICP Exercise 2.8 by Yuzhe Wu, 9 Dec 2017

;;; Using reasoning analogous to Alyssa's, describe how the difference
;;; of two intervals may be computed. Define a corresponding subtraction
;;; procedure, called sub-interval.



(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))


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
(define x (make-interval 2 3))
(define y (make-interval 11 12))

(print-interval (sub-interval x x)) ; -> [-1,1]
(print-interval (sub-interval y x)) ; -> [8,10]