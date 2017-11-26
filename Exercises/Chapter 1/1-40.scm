;;; SICP Exercise 1.40 by Yuzhe Wu, 26 Nov 2017

;;; Define a procedure cubic that can be used together with the newtons-
;;; method procedure in expressions of the form
;;; 
;;; (newtons-method (cubic a b c) 1)
;;; 
;;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.


;; procedure for constructing cubic
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;; newtons-method from the book

;; differentiation operator
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

;; Newton's method as fixed-point process
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; fixed-point procedure
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; ====== TEST ======

;; test on the cubic x^3 + 3x^2 - 7x - 6
(newline)(display "finding zero near to x^3 + 3x^2 - 7x - 6...")(newline)
(display (newtons-method (cubic 3 -7 -6) 1))





