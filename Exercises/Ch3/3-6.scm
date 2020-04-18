;;; SICP Exercise 3.6 by Yuzhe Wu, 18 Apr 2020


(define rand
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
	    ((eq? m 'reset) reset)
	    (else (error "Unknown request: RAND" m))))
    dispatch))

