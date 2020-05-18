;;; SICP Exercise 3.30 by Yuzhe Wu, 18 May 2020


(define (ripple-carry-adder a_k b_k s_k c)
  (cond ((not (and (= (length a_k) (length b_k))
		   (= (length a_k) (length c_k))))
	 (error "Input list length not equal:" 
		(length a_k)
		(length b_k)
		(length c_k)))
	((= (length a_k) 0)
	 (set-signal! c 0))
	(else 
	 (let ((c_in (make-wire)))
	   (ripple-carry-adder (cdr a_k) (cdr b_k) (cdr c_k) (cdr s_k) c_in)
	   (full-adder (car a_k) (car b_k) c_in (car s_k) c)
	   'ok))))

;; The delay of an n-bit ripple-carry adder equals 
;; n * [max(or-gate-delay, and-gate-delay + inverter-delay) + 
;; 2 * and-gate-delay + or-gate-delay]
      