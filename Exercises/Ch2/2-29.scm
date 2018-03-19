;;; SICP Exercise 2.29 by Yuzhe Wu, 19 Mar 2018

;;; A binary mobile consists of two branches, a left branch and a right branch.
;;; Each branch is a rod of a certain length, from which hangs either a weight
;;; or another binary mobile. We can represent a binary mobile using compound
;;; data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;;; A branch is constructed from a length (which must be a number) together with
;;; a structure, which may be either a number (representing a simple weight) or
;;; another mobile:

(define (make-branch length structure)
  (list length structure))



;;; a. Write the corresponding selectors left-branch and right-branch, which
;;;    return the branches of a mobile, and branch-length and branch-structure,
;;;    which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


;; TEST
(define mobile-1
  (make-mobile (make-branch 3 7)
	       (make-branch 7 3)))

(branch-structure (left-branch mobile-1)) ; -> 7
(branch-length (right-branch mobile-1))   ; -> 7



;;; b. Using your selectors, define a procedure total-weight that returns the
;;;    total weight of a mobile.

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile))))))


;; TEST
(define mobile-2
  (make-mobile (make-branch 2 mobile-1)
	       (make-branch 5 4)))

(total-weight mobile-1) ; -> 10
(total-weight mobile-2) : -> 14



;;; c. A mobile is said to be balanced if the torque applied by its top-left
;;;    branch is equal to that applied by its top-right branch and if each of
;;;    the submobiles hanging off its branches is balanced. Design a predicate
;;;    that tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (= (* (branch-length (left-branch mobile))
		 (total-weight (branch-structure (left-branch mobile))))
	      (* (branch-length (right-branch mobile))
		 (total-weight (branch-structure (right-branch mobile)))))
	   (balanced? (branch-structure (left-branch mobile)))
	   (balanced? (branch-structure (right-branch mobile))))))


;; TEST
(define mobile-3
  (make-mobile (make-branch 1 8)
	       (make-branch 3 2)))

(define mobile-4
  (make-mobile (make-branch 5 mobile-2)
	       (make-branch 7 mobile-3)))

(balanced? mobile-1) ; -> #t
(balanced? mobile-2) ; -> #t
(balanced? mobile-3) ; -> #f
(balanced? mobile-4) ; -> #f



;;; d. Suppose we change the representation of mobiles so that the constructors
;;;    are
;;;
;;;    (define (make-mobile left right) (cons left right))
;;;    (define (make-branch length structure)
;;;      (cons length structure))
;;;
;;;    How much do you need to change your programs to convert to the new
;;;    representation?

;; (Answer to part d) It suffices to rewrite the selectors in part a.
