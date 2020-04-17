;;; SICP Exercise 3.4 by Yuzhe Wu, 18 Apr 2020


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
  (define dispatch-with-passwd
    (let ((wrong-passwd-count 0))
      (lambda (p m)
	(if (eq? p passwd)
	    (begin (set! wrong-passwd-count 0) 
		   (dispatch m))
	    (begin (set! wrong-passwd-count
			 (1+ wrong-passwd-count))
		   (if (> wrong-passwd-count 7)
		       (call-the-cops)
		       "nothing")
		   (lambda (x) "Incorrect password"))))))
  dispatch-with-passwd)

