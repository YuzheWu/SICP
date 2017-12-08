;;; SICP Exercise 1.45 by Yuzhe Wu, 26 Nov 2017

;;; We saw in section 1.3.3 that attempting to compute square roots by
;;; naively finding a fixed point of y -> x/y does not converge, and
;;; that this can be fixed by average damping. The same method works for
;;; finding cube roots as fixed points of the average-damped y -> x/y^2.
;;; Unfortunately, the process does not work for fourth roots -- a sin-
;;; gle average damp is not enough to make a fixed-point search for y ->
;;; x/y^3 converge. On the other hand, if we average damp twice (i.e.,
;;; use the average damp of the average damp of y -> x/y^3 ) the fixed-
;;; point search does converge. Do some experiments to determine how
;;; many average damps are required to compute nth roots as a fixed-
;;; point search based upon repeated average damping of y -> x/y^n-1.
;;; Use this to implement a simple procedure for computing nth roots
;;; using fixed-point, average-damp, and the repeated procedure of exer-
;;; cise 1.43. Assume that any arithmetic operations you need are avail-
;;; able as primitives.


;;; Exeriment results show that in general, computing 2^n-th root re-
;;; quires n average damps to make the corresponding fixed-point search
;;; converge. In light of this obsersation, a fixed-point based method
;;; for computing n-th root is provided below.

;; nth root procedure
(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
			    (repeated average-damp
				      (floor (/ (log n) (log 2))))
			    1.0))

;;; Below are auxiliary procedures from SICP as well as from previous
;;; exercises.

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

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

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

(define (repeated f n)
  (define (iter k result)
    (if (> k n)
	result
	(iter (+ k 1) (compose f result))))
  (iter 2 f))

(define (compose f g)
  (lambda (x) (f (g x))))
