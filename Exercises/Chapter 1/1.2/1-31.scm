;;; SICP Exercise 1.31 by Yuzhe Wu, 24 Nov 2017

;;; Write a procedure called product that returns the product of the
;;; values of a function at points over a given range. Show how to 
;;; define factorial interms of product. Also use product to compute
;;; approximations to pi. Write both a recursive and an iterative 
;;; version of product.

;; recursive version of product
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product-rec term (next a) next b))))

;; iterative version of product 
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

;; pick a version
(define product product-rec)
; (define product product-iter)

;; implement factorial with product
(define (identity x) x)
(define (factorial n)
  (product identity 1 1+ n))

;; implement pi approximation with product
(define (pi-approx n)
  (define (pi-term x)
    (* (/ (-1+ x) x) (/ (1+ x) x)))
  (define (pi-next x) (+ x 2))
  (* (product pi-term 3.0 pi-next n)
     4))
