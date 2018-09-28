;;; SICP Exercise 2.33 by Yuzhe Wu, 15 June 2018

;;; Fill in the missing expressions to complete the following definitions of
;;; some basic list-manipulation operations as accumulations:
;;; (define (map p sequence)
;;;   (accumulate (lambda (x y) <??>) nil sequence))
;;; (define (append seq1 seq2)
;;;   (accumulate cons <??> <??>))
;;; (define (length sequence)
;;;   (accumulate <??> 0 sequence))



(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;;; ====== TEST ======

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(map 1+ (list 1 2 3))          ; -> (2 3 4)
(append (list 1 2) (list 3 4)) ; -> (1 2 3 4)
(length (list 1 2 3 4))        ; -> 4
