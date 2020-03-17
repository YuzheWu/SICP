;;; SICP Exercise 2.83 by Yuzhe Wu, 14 Mar 2020


;; to be included in the Scheme-number package
(define (raise x)
  (make-rational x 1))
(put 'raise '(scheme-number) raise)

;; to be included in the rational package
(define (raise x)
  (make-complex-from-real-imag x 0))
(put 'raise '(rational) raise)

