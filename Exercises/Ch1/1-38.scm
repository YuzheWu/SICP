;;; SICP Exercise 1.38 by Yuzhe Wu, 25 Nov 2017

;;; In 1737, the Swiss mathematician Leonhard Euler published a memoir
;;; De Fractionibus Continuis, which included a continued fraction ex-
;;; pansion for e - 2, where e is the base of the natural logarithms. In
;;; this fraction, the Ni are all 1, and the Di are successively 1, 2,
;;; 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-
;;; frac procedure from exercise 1.37 to approximate e, based on Euler's
;;; expansion.


;; Euler's expansion for e - 2
(define (euler-expand n)
  (cont-frac (lambda (x) 1.0)
	     (lambda (x)
	       (let ((r (remainder x 3)))
		 (if (or (= r 1)
			 (= r 0))
		     1.0
		     (* (/ (+ x 1) 3) 2.0))))
	     n))

;; cont-frac procedure from exercise 1.37
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
	result
	(iter (- i 1)
	            (/ (n i) (+ (d i) result)))))
  (iter k 0))

;; approximate e - 2
(define (approx-e-minus-two k)
  (define (iter i)
    (cond ((> i k)
	      (display "END OF APPROXIMATION"))
	    (else
	        (display i)(display " term(s): ")
		   (display (euler-expand i))(newline)
		      (iter (+ i 1)))))
  (iter 1))

(newline)(display "approximating e - 2...")(newline)
(display "exact value: ")(display (euler-expand 10000))(newline)

(approx-e-minus-two 20)
