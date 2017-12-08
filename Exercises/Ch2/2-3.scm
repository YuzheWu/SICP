;;; SICP Exercise 2.3 by Yuzhe Wu, 8 Dec 2017

;;; Implement a representation for rectangles in a plane. (Hint: You may
;;; want to make use of exercise 2.2.) In terms of your constructors and
;;; selectors, create procedures that compute the perimeter and the area
;;; of a given rectangle. Now implement a different representation for
;;; rectangles. Can you design your system with suitable abstraction
;;; barriers, so that the same perimeter and area procedures will work
;;; using either representation?



;; procedures for computing perimeter and area
;; we assume standard interface for accesing two of the sides that form
;; a right angle
(define (perimeter rect)
  (* (+ (length (first-side rect))
	(length (second-side rect)))
     2))

(define (area rect)
  (* (length (first-side rect))
     (length (second-side rect))))

(define (length seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (sqrt (+ (square (- (x-point start)
			(x-point end)))
	     (square (- (y-point start)
			(y-point end)))))))

;; point and segment packages from exercise 2.2
;; constructor and selectors For segments
(define (make-segment p q) (cons p q))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;; constructor and selectors for points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;; 1st implementation for representing rectangles
;; in terms of ordered triplet of vertices
(define (make-rect p1 p2 p3)
  (cons p1 (cons p2 p3)))

(define (first-side rect)
  (make-segment (car rect)
		(car (cdr rect))))

(define (second-side rect)
  (make-segment (car (cdr rect))
		(cdr (cdr rect))))

;; test cases for 1st implementation
(define p1 (make-point 0 0))
(define p2 (make-point 6 2))
(define p3 (make-point 5 5))

(display (perimeter (make-rect p1 p2 p3)))  ; -> 18.97
(display (area (make-rect p1 p2 p3)))       ; -> 20
(display (perimeter (make-rect p1 p2 p2)))  ; -> 12.65
(display (area (make-rect p1 p2 p2)))       ; -> 0

;; 2nd implementation for representing rectangles
;; in terms of two of its sides that form a right angle
(define (make-rect seg1 seg2) (cons seg1 seg2))
(define (first-side rect) (car rect))
(define (second-side rect) (cdr rect))

;; test cases for 2nd implementation
(define seg1 (make-segment p1 p2))
(define seg2 (make-segment p2 p3))
(define seg3 (make-segment p2 p2))

(display (perimeter (make-rect seg1 seg2))) ; -> 18.97
(display (area (make-rect seg1 seg2)))      ; -> 20
(display (perimeter (make-rect seg1 seg3))) ; -> 12.65
(display (area (make-rect seg1 seg3)))      ; -> 0
