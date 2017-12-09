;;; SICP Exercise 2.5 by Yuzhe Wu, 9 Dec 2017

;;; Show that we can represent pairs of nonnegative integers using only
;;; numbers and arithmetic operations if we represent the pair a and b
;;; as the integer that is the product 2^a 3^b. Give the corresponding
;;; definitions of the procedures cons, car, and cdr.



;;; This representation works because both 2 and 3 are prime numbers and
;;; prime factorization of integers is unique.

(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car z) (factor z 2))
(define (cdr z) (factor z 3))

;; auxiliary procedure
(define (factor n p)
  (define (iter divisor result)
    (if (> (modulo n divisor) 0)
	result
	(iter (* divisor p) (+ result 1))))
  (iter p 0))


;;; ====== TEST ======
(newline)(display (car (cons 0 0)))    ; -> 0
(newline)(display (cdr (cons 0 0)))    ; -> 0
(newline)(display (car (cons 5 0)))    ; -> 5
(newline)(display (cdr (cons 134 77))) ; -> 77
