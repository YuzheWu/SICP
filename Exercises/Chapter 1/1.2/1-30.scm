;;; SICP Exercise 1.30 by Yuzhe Wu, 24 Nov 2017

;;; Rewrite the linearly recursive sum procedure as an iteration.

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))
