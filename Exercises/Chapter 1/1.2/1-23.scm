;;; SICP Exercise 1.23 by Yuzhe Wu, 22 Nov 2017

;;; The smallest-divisor procedure does lots of needless testing:
;;; there is no need to check divisibility by larger even numbers
;;; after checking for 2. Modify the procedure and run the same
;;; timed-prime-test as in exercise 1.22. Is the runtime halved
;;; as expected?



;; more efficient auxiliary procedure for primality test
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; skip even numbers larger than 2
(define (next x)
  (if (= x 2) 3 (+ x 2)))

;; primality test by finding divisors
(define (prime? n)
  (= n (smallest-divisor n)))

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

;;; The observed ratio of speeds is roughly 1.5 instead of 2. One
;;; possible explanation is that checking divisibility by even
;;; numbers for an odd number takes less time than average.

;;; Correction: slower than expected speed is mainly due to extra
;;; IF test in NEXT procedure.
