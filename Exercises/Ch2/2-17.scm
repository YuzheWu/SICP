;;; SICP Exercise 2.17 by Yuzhe Wu, 9 Jan 2018

;;; Define a procedure last-pair that returns the list that contains only the
;;; last element of a given (nonempty) list:
;;;
;;; (last-pair (list 23 72 149 34))
;;; (34)



;; last-pair: get the last element of the list and form a singleton list
(define (last-pair items)
  (if (< (length items) 2)
      items
      (last-pair (cdr items))))


;; TEST
(last-pair (list 23 72 149 34))  ; -> (34)
(last-pair (list))               ; -> ()
(last-pair (list 0))             ; -> (0)