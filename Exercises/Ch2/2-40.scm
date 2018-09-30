;;; SICP Exercise 2.40 by Yuzhe Wu, 30 Sept 2018

;;; Define a procedure unique-pairs that, given an integer n, generates the
;;; sequence of pairs (i, j) with 1 <= j < i <= n. Use unique-pairs to simplify
;;; the definition of prime-sum-pairs given above.



;; procedure for generating sequence of pairs in the interval [1, n]
(define (enumerate-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 2 n)))

;; prime-sum-pairs simplified
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (enumerate-pairs n))))

;; auxiliary procedures
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


;;; ====== TEST ======
(enumerate-pairs 3) ; -> ((2 1) (3 1) (3 2))
(prime-sum-pairs 3) ; -> ((2 1 3) (3 2 5))

