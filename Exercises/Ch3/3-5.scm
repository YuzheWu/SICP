;;; SICP Exercise 3.5 by Yuzhe Wu, 18 Apr 2020


(define (estimate-integral P x1 x2 y1 y2 n-trials)
  (define (test)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (P x y)))
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo n-trials test)))

(define (estimate-pi n-trials)
  (define (P x y)
    (<= (+ (* x x) (* y y)) 1))
  (estimate-integral P -1.0 1.0 -1.0 1.0 n-trials))