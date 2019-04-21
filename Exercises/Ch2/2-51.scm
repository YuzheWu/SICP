;;; SICP Exercise 2.51 by Yuzhe Wu, 21 Apr 2019

;;; Define the `below` operation for painters. `below` takes two painters as
;;; arguments. The resulting painter, given a frame, draws with the first
;;; painter in the bottom of the frame and with the second painter in the top.
;;; Define `below` in two different ways—first by writing a procedure that is
;;; analogous to the `beside` procedure given above, and again in terms of
;;; `beside` and suitable rotation operations (from Exercise 2.50).



;; first implementation (analogous to `beside`)
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            split-point))
          (paint-upper
           (transform-painter
            painter2
            split-point
            (make-vect 1 0.5)
            (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-upper frame)))))

;; second implementation (in terms of `beside` and rotation operations)
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
