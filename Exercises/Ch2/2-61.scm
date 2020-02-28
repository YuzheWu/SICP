;;; SICP Exercise 2.61 by Yuzhe Wu, 26 Feb 2020


(define (adjoin-set x set)
  (define (aux left right)
    (cond ((null? right) (append left (list x)))
	  ((= x (car right)) (append left right))
	  ((< x (car right)) (append left (list x) right))
	  (else (aux (append left (list (car right))) (cdr right)))))
  (aux '() set))