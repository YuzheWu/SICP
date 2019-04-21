;;; SICP Exercise 2.50 by Yuzhe Wu, 21 Apr 2019

;;; Define the transformation `flip-horiz`, which flips painters horizontally,
;;; and transformations that rotate painters counterclockwise by 180 degrees and
;;; 270 degrees.



;; flip-horiz
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;; rotate 180 degrees counterclockwise
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

;; rotate 270 degrees counterclockwise
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))
