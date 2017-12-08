;;; SICP Exercise 2.1 by Yuzhe Wu, 8 Dec 2017

;;; Define a better version of make-rat that handles both positive and
;;; negative arguments. Make-rat should normalize the sign so that if
;;; the rational number is positive, both the numerator and denominator
;;; are positive, and if the rational number is negative, only the num-
;;; erator is negative.



(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (- (/ n g))
	      (abs (/ d g)))
	(cons (/ n g)
	      (abs (/ d g))))))
