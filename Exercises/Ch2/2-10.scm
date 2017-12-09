;;; SICP Exercise 2.10 by Yuzhe Wu, 9 Dec 2017

;;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;;; shoulder and comments that it is not clear what it means to divide
;;; by an interval that spans zero. Modify Alyssa's code to check for
;;; this condition and to signal an error if it occurs.



(define (div-interval x y)
  (define (contain-zero? z)
    (and (not (positive? (lower-bound z)))
	 (not (negative? (upper-bound z)))))
  (if (contain-zero? y)
      (display "error: division by interval spanning zero")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))


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
(define x (make-interval -1 1))
(define y (make-interval 2 4))

(print-interval (div-interval x y)) ; -> [-.5,.5]
(newline)(div-interval y x)         ; error