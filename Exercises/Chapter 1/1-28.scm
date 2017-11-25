;;; SICP Exercise 1.28 by Yuzhe Wu, 22 Nov 2017

;;; Modify the expmod procedure to signal if it discovers a nontrivial
;;; square root of 1, and use this to implement the Miller-Rabin test
;;; with a procedure analogous to fermat-test. Check your procedure by
;;; testing various known primes and non-primes. Hint: One convenient
;;; way to make expmod signal is to have it return 0.

;; miller-robin test with a random number
(define (miller-robin-test n)
  (define (try-it a)
    (= (expmod a (-1+ n) n) 1))
  (try-it (1+ (random (-1+ n)))))

;; expmod with signal on occurrence of sqrt of 1 modulo n
(define (expmod base exp n)
  (cond ((= exp 0) 1)
	((even? exp)
	 (square-mod-signal
	  (expmod base (/ exp 2) n) n))
	(else
	 (remainder (* base (expmod base (- exp 1) n))
		    n))))

;; compute square modulo n, signal nontrivial sqrt of 1
;; with return value of 0
(define (square-mod-signal x n)
  (define r (remainder (square x) n))
  (if (and (> x 1)
	   (< x (-1+ n))
	   (= r 1))
      0
      r))

;; fast primality test by performing random Miller-Robin tests
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-robin-test n) (fast-prime? n (-1+ times)))
	(else false)))

;; wrap it up, use 10 Miller-Robin tests
(define (prime? n)
  (fast-prime? n 10))

;;; --- TEST ---

(define (test n)
  (display "Performing Miller-Robin test on ")
  (display n)(display "...")(newline)
  (cond ((prime? n)
	  (display "Test passed")(newline))
	(else
	  (display "Test failed")(newline))))

(newline)
;; test on Carmichael numbers
(display "--- TESTING CARMICHAEL NUMBERS ---")(newline)
(test 561)(newline)
(test 1105)(newline)
(test 1729)(newline)
(test 2465)(newline)
(test 2821)(newline)
(test 6601)(newline)

;; test on primes
(display "--- TESTING PRIMES ---")(newline)
(test 1009)(newline)
(test 1013)(newline)
(test 1019)(newline)
(test 10007)(newline)
(test 10009)(newline)
(test 10037)(newline)

(display "- END OF TEST")
