;;; SICP Exercise 2.28 by Yuzhe Wu, 19 Mar 2018

;;; Write a procedure fringe that takes as argument a tree (represented as a
;;; list) and returns a list whose elements are all the leaves of the tree
;;; arranged in left-to-right order.



;; implement fringe
(define (fringe items)
  (cond ((null? items) '())
	((not (pair? items)) (list items))
	(else (append (fringe (car items))
		      (fringe (cdr items))))))


;; TEST
(define x (list (list 1 2) (list 3 4)))
(fringe x)          ; -> (1 2 3 4)
(fringe (list x x)) ; -> (1 2 3 4 1 2 3 4)