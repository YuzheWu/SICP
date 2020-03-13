;;; SICP Exercise 2.75 by Yuzhe Wu, 12 Mar 2020


(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)