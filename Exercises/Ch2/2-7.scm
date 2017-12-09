;;; SICP Exercise 2.7 by Yuzhe Wu, 9 Dec 2017

;;; Alyssa's program is incomplete because she has not specified the
;;; implementation of the interval abstraction. Here is a definition of
;;; the interval constructor:
;;;
;;; (define (make-interval a b) (cons a b))
;;; 
;;; Define selectors upper-bound and lower-bound to complete the implem-
;;; entation.



(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))


;;; ====== TEST ======

;; constructor 
(define (make-interval a b) (cons a b))

;; test cases
(newline)(display (upper-bound (make-interval 0 0)))  ; -> 0
(newline)(display (lower-bound (make-interval 0 0)))  ; -> 0
(newline)(display (upper-bound (make-interval -2 3))) ; -> 3
(newline)(display (lower-bound (make-interval -2 3))) ; -> -2