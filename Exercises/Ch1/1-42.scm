;;; SICP Exercise 1.42 by Yuzhe Wu, 26 Nov 2017

;;; Let f and g be two one-argument functions. The composition f after g
;;; is defined to be the function x -> f(g(x)). Define a procedure com-
;;; pose that implements composition. For example, if inc is a procedure
;;; that adds 1 to its argument,
;;;
;;; ((compose square inc) 6)
;;; 49


;; implement composition
(define (compose f g)
  (lambda (x) (f (g x))))


;;; ====== TEST ======

;; square procedure
(define (square x) (* x x))

;; inc procedure
(define (inc x) (+ x 1))

;; evaluate the expression in question; expect 49
((compose square inc) 6)
