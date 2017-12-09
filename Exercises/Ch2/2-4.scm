;;; SICP Exercise 2.4 by Yuzhe Wu, 9 Dec 2017

;;; Here is an alternative procedural representation of pairs. For this
;;; representation, verify that (car (cons x y)) yields x for any
;;; objects x and y.
;;;
;;; (define (cons x y)
;;;   (lambda (m) (m x y)))
;;; (define (car z)
;;;   (z (lambda (p q) p)))
;;;
;;; What is the corresponding definition of cdr? (Hint: To verify that
;;; this works, make use of the substitution model of section 1.1.5.)



;; Below is a sequence of expressions that (car (cons x y)) reduces to
;; according to the substitution model
;;
;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; definition fo cdr
(define (cdr z)
  (z (lambda (p q) q)))
