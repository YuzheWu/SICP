;;; SICP Exercise 1.41 by Yuzhe Wu, 26 Nov 2017

;;; Define a procedure double that takes a procedure of one argument as
;;; argument and returns a procedure that applies the original procedure
;;; twice. For example, if inc is a procedure that adds 1 to its argu-
;;; ment, then (double inc) should be a procedure that adds 2. What
;;; value is returned by
;;;
;;; (((double (double double)) inc) 5)


;; implement the double procedure
(define (double f)
  (lambda (x) (f (f x))))

;; define the inc procedure
(define (inc x) (+ x 1))

;; evaluate the expression in question
(((double (double double)) inc) 5)

;;; The above expression evaluate to 21. 
