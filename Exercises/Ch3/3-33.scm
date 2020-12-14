;;; SICP Exercise 3.33 by Yuzhe Wu, 12 Dec 2020


(define (averager a b c)
  (let ((u (make-connector))
	(v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))