;;; SICP Exercise 2.21 by Yuzhe Wu, 17 Mar 2018

;;; The procedure square-list takes a list of numbers as argument and returns a
;;; list of the squares of those numbers.
;;;
;;; Here are two different definitions of square-list. Complete both of them by
;;; filling in the missing expressions:
;;;
;;; (define (square-list items)
;;;   (if (null? items)
;;;       nil
;;;       (cons <??> <??>)))
;;; (define (square-list items)
;;;   (map <??> <??>))



(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))


;; TEST
(square-list (list 1 2 3 4))   ; -> (1 4 9 16)
(square-list-2 (list 1 2 3 4)) ; -> (1 4 9 16)