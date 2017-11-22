;;; SICP Exercise 1.24 by Yuzhe Wu, 22 Nov 2017

;;; Modify the timed-prime-test procedure of exercise 1.22 to use fast-
;;;prime? (the Fermat method), and test each of the 12 primes you found
;;; in the exercise.



;; auxiliary procedure for performing the Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
		    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
		    m))))

;; choose a random number between 1 and n-1
;; then perform the Fermat test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; fast primality test by performing random Fermat tests
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; wrap it up, use 10 Fermat tests
(define (prime? n)
  (fast-prime? n 10))

;; record time elapsed after running proc n times
(define (time-it proc n)
  (define (iter n start)
    (cond ((= n 0) (- (runtime) start))
	    (else
	        (proc)
		   (iter (-1+ n) start))))
  (iter n (runtime)))

;; if prime, display number and time elapsed for
;; performing 1000 primality tests on that number
(define loop 1000)

(define (timed-prime-test n)
  (define (test)
    (prime? n))
  (if (prime? n)
      (report-prime n (time-it test loop))))

(define (report-prime p elapsed-time)
  (display p)(display " *** ")(display elapsed-time)(newline))

;; search for primes in range [low, high], display
;; primes found along with search time
(define (search-for-primes low high)
  (define (end-of-search)
    (display "- END OF SEARCH -")(newline))
  (cond ((> low high) (end-of-search))
  ((even? low) (search-for-primes (1+ low) high))
  (else
   (timed-prime-test low)
   (search-for-primes (+ low 2) high))))

;;; --- TEST ---

;; run search procedure
(newline)

(display "Searching primes from 1000...")(newline)
(search-for-primes 1000 1030)(newline)

(display "Searching primes from 10000...")(newline)
(search-for-primes 10000 10040)(newline)

(display "Searching primes from 100000...")(newline)
(search-for-primes 100000 100050)(newline)

(display "Searching primes from 1000000...")(newline)
(search-for-primes 1000000 1000060)(newline)

(display "- END OF TEST")

;;; Testing primes near 1,000,000 takes roughly 1.6 times longer than
;;; testing those near 1000. Causes for discrepancy from prediction by
;;; logarithmic growth rate are unclear.
