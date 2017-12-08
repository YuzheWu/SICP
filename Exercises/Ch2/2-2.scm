;;; SICP Exercise 2.2 by Yuzhe Wu, 8 Dec 2017

;;; Consider the problem of representing line segments in a plane. Each
;;; segment is represented as a pair of points: a starting point and an
;;; ending point. Define a constructor make-segment and selectors start-
;;; segment and end-segment that define the representation of segments
;;; in terms of points. Furthermore, a point can be represented as a
;;; pair of numbers: the x coordinate and the y coordinate. Accordingly,
;;; specify a constructor make-point and selectors x-point and y-point
;;; that define this representation. Finally, using your selectors and
;;; constructors, define a procedure midpoint-segment that takes a line
;;; segment as argument and returns its midpoint (the point whose coord-
;;; inates are the average of the coordinates of the endpoints).



;; constructor and selectors for segments
(define (make-segment p q) (cons p q))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;; constructor and selectors for points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; compute midpoint
(define (midpoint-segment seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (make-point (average (x-point start)
			 (x-point end))
		(average (y-point start)
			 (y-point end)))))

(define (average x y) (/ (+ x y) 2))

;; print point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


;;; ====== TEST ======

(define p1 (make-point 0. 0.))
(define p2 (make-point 2. 0.))
(define p3 (make-point 3. 4.))

(print-point (midpoint-segment (make-segment p1 p2))) ; -> (1.,0.)
(print-point (midpoint-segment (make-segment p1 p3))) ; -> (1.5,2.)
(print-point (midpoint-segment (make-segment p2 p3))) ; -> (2.5,2.)
