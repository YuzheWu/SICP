;;; SICP Exercise 2.59 by Yuzhe Wu, 26 Feb 2020


(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))
