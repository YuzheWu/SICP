;;; SICP Exercise 1.43 by Yuzhe Wu, 26 Nov 2017

;;; If f is a numerical function and n is a positive integer, then we
;;; can form the nth repeated application of f, which is defined to be
;;; the function whose value at x is f(f(...(f(x))...)). For example, if
;;; f is the function x -> x + 1, then the nth repeated application of f
;;; is the function x -> x + n. If f is the operation of squaring a num-
;;; ber, then the nth repeated application of f is the function that
;;; raises its argument to the 2^n-th power. Write a procedure that
;;; takes as inputs a procedure that computes f and a positive integer n
;;; and returns the procedure that computes the nth repeated application
;;; of f. Your procedure should be able to be used as follows:
;;;
;;; ((repeated square 2) 5)
;;; 625
;;;
;;; Hint: You may find it convenient to usecomposefrom exercise 1.42.


;; implement nth repeated function application
(define (repeated f n)
  (define (iter k result)
    (if (> k n)
	result
	(iter (+ k 1) (compose f result))))
  (iter 2 f))

;; compose procedure from exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


;;; ====== TEST ======

;; square procedure
(define (square x) (* x x))

;; evaluate the expression in question; expect 625
((repeated square 2) 5)