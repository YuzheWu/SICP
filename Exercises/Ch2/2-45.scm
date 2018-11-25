;;; SICP Exercise 2.45 by Yuzhe Wu, 25 Nov 2018

;;; right-split and up-split can be expressed as instances of a general split-
;;; ting operation. Define a procedure split with the property that evaluating
;;;
;;; (define right-split (split beside below))
;;; (define up-split (split below beside))
;;;
;;; produces procedures right-split and up-split with the same behaviors as the
;;; ones already defined.



(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))
