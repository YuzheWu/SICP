;;; SICP Exercise 1.37 by Yuzhe Wu, 25 Nov 2017

;;; Suppose that n and d are procedures of one argument (the term index
;;; i) that return the Ni and Di of the terms of the continued fraction.
;;; Define a procedure cont-frac such that evaluating (cont-frac n d k)
;;; computes the value of the k-term finite continued fraction. Check
;;; your procedure by approximating 1/phi (inverse of golden ratio) using
;;;
;;; (cont-frac (lambda (i) 1.0)
;;;            (lambda (i) 1.0)
;;;    k)
;;;
;;; for successive values of k. How large must you make k in order to get
;;; an approximation that is accurate to 4 decimal places?
;;;
;;; If your cont-frac procedure generates a recursive process, write one
;;; that generates an iterative process. If it generates an iterative
;;; process, write one that generates a recursive process.


;; recursive version of cont-frac
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i (+ k 1))
	0
	(/ (n i)
	   (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; iterative version of cont-frac
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
	result
	(iter (- i 1)
	      (/ (n i) (+ (d i) result)))))
  (iter k 0))

;; pick a version
(define cont-frac cont-frac-iter)
; (define cont-frac cont-frac-rec)

;; approximate the golden ratio
(define (golden-ratio k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

(define (approx-golden-ratio k)
  (define (iter i)
    (cond ((> i k)
	   (display "END OF APPROXIMATION"))
	  (else
	   (display i)(display " term(s): ")
	   (display (golden-ratio i))(newline)
	   (iter (+ i 1)))))
  (iter 1))

(newline)(display "approximating the golden ratio...")(newline)
(display "exact value: ")(display (golden-ratio 10000))(newline)

(approx-golden-ratio 20)

;;; We get an approximation accurate to 4 decimal places if k > 10.
