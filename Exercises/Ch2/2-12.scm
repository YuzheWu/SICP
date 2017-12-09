;;; SICP Exercise 2.12 by Yuzhe Wu, 9 Dec 2017

;;; Define a constructor make-center-percent that takes a center and a
;;; percentage tolerance and produces the desired interval. You must
;;; also define a selector percent that produces the percentage tolerance
;;; for a given interval. The center selector is the same as the one
;;; shown above.



(define (make-center-percent center percentage)
  (let ((width (abs (* center percentage 0.01))))
    (make-interval (- center width)
		   (+ center width))))

(define (percent interval)
  (let ((center (average (lower-bound interval) (upper-bound interval)))
	(width (/ (- (upper-bound interval) (lower-bound interval)) 2)))
    (* (abs (/ width center)) 100)))

(define (average x y)
  (/ (+ x y) 2))


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
(print-interval (make-center-percent 10 5)) ; -> [9.5,10.5]
(print-interval (make-center-percent 10 0)) ; -> [10,10]

(newline)(display (percent (make-center-percent 7 7)))   -> 7
(newline)(display (percent (make-center-percent 100 7))) -> 7
