;;; SICP Exercise 1.22 by Yuzhe Wu, 22 Nov 2017

;;; Using the procedure timed-prime-test, write a procedure search-
;;; for-primes that checks the primality of consecutive odd integers
;;; in a specified range. Use your procedure to find the three small-
;;; est primes larger than 1000; larger than 10,000; larger than
;;; 100,000; larger than 1,000,000. Note the time needed to test each
;;; prime.



;; auxiliary procedure for primality test
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (1+ test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

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

;;; As the prime number to be test grows 10 times in size, test time
;;; roughly increases by a factor of sqrt(10) as expected from the
;;; algorithm complexity.
