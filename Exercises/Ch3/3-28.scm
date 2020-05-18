;;; SICP Exercise 3.28 by Yuzhe Wu, 18 May 2020


(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
	((and (= a1 1) (= a2 0)) 1)
	((and (= a1 0) (= a2 1)) 1)
	((and (= a1 1) (= a2 1)) 1)
	(else (error "Invvalid signal" a1 a2))))