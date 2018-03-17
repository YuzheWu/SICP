;;; SICP Exercise 2.23 by Yuzhe Wu, 17 Mar 2018

;;; The procedure for-each is similar to map. It takes as arguments a procedure
;;; and a list of elements. How- ever, rather than forming a list of the
;;; results, for-each just applies the procedure to each of the elements in
;;; turn, from left to right. The values returned by applying the procedure to
;;; the elements are not used at all - for-each is used with procedures that
;;; perform an action, such as printing. Give an implementation of for-each.



(define (my-for-each proc items)
  (cond ((null? items) #t)
	(else (proc (car items))
	      (my-for-each proc (cdr items)))))


;; TEST
(my-for-each (lambda (x)       ; 57
	       (newline)       ; 321
	       (display x))    ; 88
	     (list 57 321 88))

