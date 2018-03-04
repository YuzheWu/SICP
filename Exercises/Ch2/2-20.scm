;;; SICP Exercise 2.20 by Yuzhe Wu, 4 Mar 2018

;;; The procedures +, *, and list take arbitrary numbers of arguments. One way
;;; to define such procedures is to use define with dotted-tail notation. In a
;;; procedure definition, a parameter list that has a dot before the last para-
;;; meter name indicates that, when the procedure is called, the initial para-
;;; meters (if any) will have as values the initial arguments, as usual, but the
;;; final s value will be a list of any remaining arguments. For
;;; instance, given the definition
;;;
;;; (define (f x y . z) <body>)
;;;
;;; the procedure f can be called with two or more arguments. If we evaluate
;;;
;;; (f 1 2 3 4 5 6)
;;;
;;; then in the body of f, x will be 1, y will be 2, and z will be the list
;;; (3 4 5 6).
;;;
;;; Use this notation to write a procedure same-parity that takes one or more
;;; integers and returns a list of all the arguments that have the same even-odd
;;; parity as the first argument. For example,
;;;
;;; (same-parity 1 2 3 4 5 6 7)
;;; (1 3 5 7)
;;; (same-parity 2 3 4 5 6 7)
;;; (2 4 6)



;; implement same-parity
(define (same-parity head . integers)
  (define (iter result remainder)
    (if (null? remainder)
	result
	(if (is-same-parity? (car result) (car remainder))
	    (iter (append result (list (car remainder)))
		  (cdr remainder))
	    (iter result (cdr remainder)))))
  (iter (list head) integers))

;; helper function that determins if two integers are of the same parity
(define (is-same-parity? a b)
  (even? (- a b)))


;; TEST
(same-parity 1)             ; -> (1)
(same-parity 1 2 3 4 5 6 7) ; -> (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; -> (2 4 6)
			     