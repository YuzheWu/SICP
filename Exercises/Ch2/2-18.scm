;;; SICP Exercise 2.18 by Yuzhe Wu, 9 Jan 2018

;;; Define a procedure reverse that takes a list as argument and returns a list
;;; of the same elements in reverse order:
;;;
;;; (reverse (list 1 4 9 16 25))
;;; (25 16 9 4 1)



;; reverse: reverse a list
(define (reverse items)
  (define (iter result remainder)
    (if (null? remainder)
	result
	(iter (cons (car remainder) result)
	      (cdr remainder))))
  (iter '() items))


;; TEST
(reverse (list 1 4 9 16 25))  ; -> (25 16 9 4 1)
(reverse '())                 ; -> ()
(reverse (list reverse))      ; -> (#[compound-procedure 22 reverse])
