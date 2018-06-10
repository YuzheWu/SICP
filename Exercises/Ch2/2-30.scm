;;; SICP Exercise 2.30 by Yuzhe Wu, 10 June 2018

;;; Define a procedure square-tree analogous to the square-list procedure of
;;; Exercise 2.21. That is, square-tree should behave as follows:
;;;
;;; (square-tree
;;;  (list 1
;;;        (list 2 (list 3 4) 5)
;;;        (list 6 7)))
;;; (1 (4 (9 16) 25) (36 49))
;;;
;;; Define square-tree both directly (i.e., without using any higher-order
;;; procedures) and also by using map and recursion.



;; direct implementation of square-tree
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; implementation using map and recursion
(define (square-tree2 tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree2 subtree)
	     (square subtree)))
       tree))


;;; ====== TEST ======

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))             
; -> (1 (4 (9 16) 25) (36 49))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; -> (1 (4 (9 16) 25) (36 49))
