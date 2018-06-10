;;; SICP Exercise 2.32 by Yuzhe Wu, 10 June 2018

;;; We can represent a set as a list of distinct elements, and we can represent
;;; the set of all subsets of the set as a list of lists. For example, if the
;;; set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3)
;;; (1 2) (1 2 3)). Complete the following definition of a procedure that
;;; generates the set of subsets of a set and give a clear explanation of why it
;;; works:
;;;
;;; (define (subsets s)
;;;   (if (null? s)
;;;       (list '())
;;;       (let ((rest (subsets (cdr s))))
;;;         (append rest (map <??> rest)))))



(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (set) 
			    (append (list (car s)) set))
			  rest)))))


;;; ====== TEST ======

(subsets (list 1 2 3)) ; -> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


;; This solution works because the set of all subsets exactly consists of those
;; subsets not containing the first element and those containing the first
;; element. The first part are precisely all subsets of the tail of the
;; original set, denoted by 'rest' is our solution. And the second part are the
;; the same sets each with the first element added.

			    











