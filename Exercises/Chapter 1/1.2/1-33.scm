;;; SICP Exercise 1.33 by Yuzhe Wu, 24 Nov 2017

;;; Write an even more general version of accumulate, called 
;;; filtered-accumulate, that combine only those terms derived from
;;; values in the range that satisfy a specified condition. Show how to
;;; express the following using filtered-accumulate:
;;;
;;; a. the sum of squares of the prime numbers in the interval a to b.
;;;
;;; b. the product of all the positive integers less than n that are 
;;; relatively prime to n.

;; implement filtered-accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner (if (filter a)
			    (term a)
			    null-value)
			result))))
  (iter a null-value))

;; Part a
(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a 1+ b))

;; Part b
(define (product-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 1+ n))


;;; AUXILIARY PROCEDURES FOR PART A

;; square
(define (square x) (* x x))

;; primality test 
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (1+ test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))

;;; AUXILIARY PROCEDURES FOR PART B

;; greatest common divisor
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; identity procedure
(define (identity x) x)