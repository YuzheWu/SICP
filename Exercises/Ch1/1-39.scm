;;; SICP Exercise 1.39 by Yuzhe Wu, 25 Nov 2017

;;; Define a procedure (tan-cf x k) that computes an approximation to
;;; the tangent function based on Lambert's formula. K specifies the
;;; number of terms to compute, as in exercise 1.37.


;; Lambder's formula for tangent approximation
(define (tan-cf x k)
  (cont-frac (lambda (i)
	       (if (= i 1) x (- (* x x))))
	     (lambda (i)
	       (- (* i 2.0) 1.0))
	     k))


;;; ====== TEST ======

;; test on tan(10)
(newline)(display "approximating tan(10)...")(newline)
(display "exact value: ")(display (tan 10))(newline)
(display "10 terms: ")(display (tan-cf 10 10))(newline)
(display "100 terms: ")(display (tan-cf 10 100))(newline)
(display "1000 terms: ")(display (tan-cf 10 1000))(newline)