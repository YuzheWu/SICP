;;; SICP Exercise 2.41 by Yuzhe Wu, 30 Sept 2018

;;; Write a procedure to find all ordered triples of distinct positive integers
;;; i, j, and k less than or equal to a given integer n that sum to a given
;;; integer s.


(define (fixed-sum-triples s n)
  (fixed-sum-tuples s n 3))

(define (fixed-sum-tuples s n k)
  (if (= k 1)
      (if (and (<= 1 s) (<= s n)) 
	  (list (list s))
	  '())
      (flatmap (lambda (i)
		 (map (lambda (p) (append p (list i)))
		      (fixed-sum-tuples (- s i) (- i 1) (- k 1))))
	       (enumerate-interval 1 n))))


;;; ====== TEST ======
(fixed-sum-triples 11 10) ; -> ((2 4 5) (2 3 6) (1 4 6) (1 3 7) (1 2 8))