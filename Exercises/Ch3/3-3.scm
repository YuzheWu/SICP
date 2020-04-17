;;; SICP Exercise 3.3 by Yuzhe Wu, 18 Apr 2020


(define (make-account balance passwd)
  (define (withdraw amount) 
    (if (>= balance amount)
	(begin (set! balance (- balance amount)) 
	       balance)
	"Insufficient funds")) 
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT"
		       m))))
  (define (dispatch-with-passwd p m)
    (if (eq? p passwd)
	(dispatch m)
	(lambda (x) "Incorrect password")))
  dispatch-with-passwd)