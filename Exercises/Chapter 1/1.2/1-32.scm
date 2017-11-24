;;; SICP Exercise 1.31 by Yuzhe Wu, 24 Nov 2017

;;; Write a procedure called product that returns the product of the
;;; values of a function at points over a given range. Show how to 
;;; define factorial interms of product. Also use product to compute
;;; approximations to pi. Write both a recursive and an iterative 
;;; version of product.

;; recursive accumulate
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner null-value term (next a) next b))))

;; iterative accumulate
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) 
	      (combiner (term a) result))))
  (iter a null-value))

;; pick a version
(define accumulate accumulate-rec)
; (define accumulate accumulate-iter)

;; implement sum
(define (sum term a next b)
  (accumulate + 0 term a next b))

;; implement product
(define (product term a next b)
  (accumulate * 1 term a next b))
