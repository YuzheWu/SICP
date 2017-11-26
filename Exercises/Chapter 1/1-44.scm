;;; SICP Exercise 1.44 by Yuzhe Wu, 26 Nov 2017

;;; The idea of smoothing a function is an important concept in signal
;;; processing. If f is a function and dx is some small number, then the
;;; smoothed version of f is the function whose value at a point x is
;;; the average of f(x - dx), f(x), and f(x + dx). Write a procedure
;;; smooth that takes as input a procedure that computes f and returns a
;;; procedure that computes the smoothed f. It is sometimes valuable to
;;; repeatedly smooth a function (that is, smooth the smoothed function,
;;; and so on) to obtain the n-fold smoothed function. Show how to gene-
;;; rate the n-fold smoothed function of any given function using smooth
;;; and repeated from exercise 1.43.


;; implement smooth
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define dx 0.1)

;; implement n-fold smooth using smooth and repeated
(define (smooth-n f n)
  ((repeated smooth n) f))

;; repeated from exercise 1.43
(define (repeated f n)
  (define (iter k result)
    (if (> k n)
	result
	(iter (+ k 1) (compose f result))))
  (iter 2 f))

;; compose procedure from exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))