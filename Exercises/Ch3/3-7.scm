;;; SICP Exercise 3.7 by Yuzhe Wu, 19 Apr 2020


(define (make-joint acc passwd new-passwd)
  (if (acc passwd 'check-passwd)
      (lambda (p m)
	(if (eq? p new-passwd)
	    (acc passwd m)
	    (lambda (x) "Incorrect passwd")))
      (error "Wrong password: MAKE-JOINT"
	     passwd)))

;; modify make-account to support explicit password check
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
    (if (eq? m 'check-passwd)
	(eq? p passwd)
	(if (eq? p passwd)
	    (dispatch m)
	    (lambda (x) "Incorrect password"))))
  dispatch-with-passwd)