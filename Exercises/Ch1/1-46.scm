;;; SICP Exercise 1.46 by Yuzhe Wu, 26 Nov 2017

;;; Several of the numerical methods described in this chapter are ins-
;;; tances of an extremely general computational strategy known as iter-
;;; ative improvement. Iterative improvement says that, to compute some-
;;; thing, we start with an initial guess for the answer, test if the
;;; guess is good enough, and otherwise improve the guess and continue
;;; the process using the improved guess as the new guess. Write a pro-
;;; cedure iterative-improve that takes two procedures as arguments: a
;;; method for telling whether a guess is good enough and a method for
;;; improving a guess. Iterative-improve should return as its value a
;;; procedure that takes a guess as argument and keeps improving the
;;; guess until it is good enough. Rewrite the sqrt procedure of section
;;; 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of
;;; iterative-improve.


;; generic iterative-improve procedure
(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (iter guess)
      (if (good-enough? guess)
	  guess
	  (iter (improve guess))))
    (iter initial-guess)))

;; implement sqrt in terms of iterative-improve
(define (sqrt x)
  ((iterative-improve (lambda (guess)
			(< (abs (- (square guess) x)) 0.001))
		      (lambda (guess)
			(/ (+ guess (/ x guess)) 2)))
   1.0))
	    
;; implement fixed-point in terms of iterative-imrove
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
			(< (abs (- guess (f guess))) tolerance))
		      f)
   first-guess))