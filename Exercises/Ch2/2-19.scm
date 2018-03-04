;;; SICP Exercise 2.19 by Yuzhe Wu, 4 Mar 2018

;;; We want to rewrite the change-counting program of Section 1.2.2 so that its
;;; second ar- gument is a list of the values of the coins to use rather than an
;;; integer specifying which coins to use. We could then have lists that defined
;;; each kind of currency:
;;;
;;; (define us-coins (list 50 25 10 5 1))
;;; (define uk-coins (list 100 50 20 10 5 2 1 0.5))
;;;
;;; We could then call cc as follows:
;;;
;;; (cc 100 us-coins)
;;; 292
;;;
;;; To do this will require changing the program cc somewhat. It will still have
;;; the same form, but it will access its second argument differently, as follows:
;;;
;;; (define (cc amount coin-values) 
;;;   (cond ((= amount 0) 1)
;;;         ((or (< amount 0) (no-more? coin-values)) 0) 
;;;         (else
;;;          (+ (cc amount
;;;                 (except-first-denomination
;;;                  coin-values))
;;;             (cc (- amount
;;;                    (first-denomination
;;;                     coin-values))
;;;                 coin-values)))))
;;;
;;; Define the procedures first-denomination, except-first-denomination, and
;;; no-more? in terms of primitive oper- ations on list structures. Does the order
;;; of the list coin- values affect the answer produced by cc? Why or why not?



;; the modified cc procedure
(define (cc amount coin-values) 
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0) 
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

;; helper functions
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define no-more? null?)

;; The answer produced by cc does not depend on the order of the list coin-values
;; because the decomposition of the calculation on which this algorithm is based
;; remains valid regardless of the particular denomination under consideration.


;;; TEST
(define us-coins (list 50 25 10 5 1))
(define us-coins-reordered (list 25 1 50 5 10))

(cc 100 us-coins)           ; -> 292
(cc 100 us-coins-reordered) ; -> 292
