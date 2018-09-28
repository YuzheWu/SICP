;;; SICP Exercise 2.39 by Yuzhe Wu, 28 Sept 2018

;;; Complete the following definitions of reverse (Exercise 2.18) in terms of
;;; fold-right and fold-left from Exercise 2.38.



(define (reverse1 sequence)
  (fold-right (lambda (x seq) (append seq (list x))) '() sequence)) 

(define (reverse2 sequence)
  (fold-left (lambda (seq x) (cons x seq)) '() sequence))


;;; ====== TEST ======

(reverse1 (list 1 2 3 4 5))   ; -> (5 4 3 2 1)
(reverse2 (list 1 2 3 4 5))   ; -> (5 4 3 2 1)