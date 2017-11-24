;;; SICP Exercise 1.29 by Yuzhe Wu, 24 Nov 2017

;;; Define a procedure that takes as arguments f, a, b and n and returns
;;; the value of the integral, computed using Simpson's Rule. Use your
;;; procedure to integrate cube between 0 and 1 (with n = 100 and n = 
;;; 1000), and compare the results to those of the naive method.

;; generic sum pattern from the book
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;; integration with Simpson's Rule, with n even
(define (integral-simpson f a b n)
  (define h (/ (- b a ) n))
  (define (term x)
    (+ (* 2.0 (f x))
       (* 4.0 (f (+ x h)))))
  (define (add-2h x)
    (+ x (* h 2)))
  (* (+ (f a)
	(* 4.0 (f (+ a h)))
	(sum term (+ a (* h 2)) add-2h (- b (* h 2)))
	(f b))
     (/ h 3)))

;;; --- TEST ---

;; naive integration method
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; test on the cube function
(define (cube x) (* x x x))

(newline)(display "Integrate cube from 0 to 1...")(newline)
(display "naive dx=0.01: ")
(display (integral cube 0 1 0.01))(newline)
(display "naive dx=0.001: ")
(display (integral cube 0 1 0.001))(newline)

(display "simpson n=100: ")
(display (integral-simpson cube 0 1 100))(newline)
(display "simpson n=1000: ")
(display (integral-simpson cube 0 1 1000))(newline)

