;;; SICP Exercise 1.27 by Yuzhe Wu, 22 Nov 2017

;;; Demonstrate that the Carmichael numbers listed in footnote 47 really
;;; do fool the Fermat test. That is, write a procedure that takes an
;;; integer n and tests whether a^n is congruent to a modulo n for every
;;; a<n, and try your procedure on the given Carmichael numbers.



;; Fermat test on n for all a < n
(define (fermat-test-all n)
  (define (iter k)
    (cond ((> k (-1+ n)) true)
	  ((= (expmod k n n) k) (iter (1+ k)))
	  (else false)))
  (iter 2))

;; auxiliary procedure for performing the Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
		        m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
		        m))))

;;; --- TEST ---

;; test on Carmichael numbers
(define (test n)
  (display "Performing Fermat test on ")
  (display n)(display "...")(newline)
  (cond ((fermat-test-all n)
	 (display "Test passed")(newline))
	(else
	 (display "Test failed")(newline))))

(newline)
(display "--- TESTING CARMICHAEL NUMBERS ---")(newline)

(test 561)(newline)
(test 1105)(newline)
(test 1729)(newline)
(test 2465)(newline)
(test 2821)(newline)
(test 6601)(newline)

(display "- END OF TEST")
