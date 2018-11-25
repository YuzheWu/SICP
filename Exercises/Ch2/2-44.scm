;;; SICP Exercise 2.44 by Yuzhe Wu, 2 Oct 2018

;;; Define the procedure up-split used by corner- split. It is similar to right-
;;; split, except that it switches the roles of below and beside. Define the
;;; procedure up-split used by corner-split. It is similar to right-split,
;;; except that it switches the roles of below and beside.



(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))
