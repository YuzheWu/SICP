;;; SICP Exercise 3.37 by Yuzhe Wu, 15 Dec 2020


(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv val)
  (let ((z (make-connector)))
    (constant val z)
    z))