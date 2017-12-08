;;; SICP Exercise 1.36 by Yuzhe Wu, 25 Nov 2017

;;; Modify fixed-point so that it prints the sequence of approximations
;;; it generates, using the newline and display primitives shown in
;;; exercise 1.22. Then find a solution to x^x = 1000 by finding a fixed
;;; point of x -> log(1000)/log(x). (Use Scheme's primitive log proce-
;;; dure, which computes natural logarithms.) Compare the number of
;;; steps this takes with and without average damping. (Note that you
;;; cannot start fixed-point with a guess of 1, as this would cause
;;; division by log(1) = 0.)


;; modified version of fixed-point that traces approximation process
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display "step ")(display count)(display ": ")(display guess)(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next (+ count 1)))))
  (try first-guess 1))

;; average damping procedure
(define (average a b)
  (/ (+ a b) 2))
(define (avg-damp f)
  (lambda (x)
    (average x (f x))))

;; find a solution to x^x = 1000, with an initial of 4
(newline)(display "finding solution to x^x = 1000...")(newline)

;; without average damping
(newline)(display "without average damping...")(newline)
(fixed-point (lambda (x)
	       (/ (log 1000) (log x)))
	     4.0)

;; with average damping
(newline)(display "with average damping...")(newline)
(fixed-point (avg-damp (lambda (x)
	       (/ (log 1000) (log x))))
	     4.0)

;;; Note that with the same initial guess of 4, it takes 29 steps with-
;;; out average damping to find a solution to x^x = 1000 within the
;;; specified tolerance, while with average damping the number of steps
;;; required reduces to only 7.
