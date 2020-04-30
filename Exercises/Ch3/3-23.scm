;;; SICP Exercise 3.22 by Yuzhe Wu, 30 Apr 2020


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (set-prev-ptr! triplet item)
  (set-car! (cdr triplet) item))
(define (set-next-ptr! triplet item)
  (set-cdr! (cdr triplet) item))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque) 
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (eeor "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-triplet (cons item (cons '() (front-ptr deque)))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-triplet)
	   (set-rear-ptr! deque new-triplet)
	   deque)
	  (else
	   (set-prev-ptr! (front-ptr deque) new-triplet)
	   (set-front-ptr! deque new-triplet)
	   deque))))
(define (rear-insert-deque! deque item)
  (let ((new-triplet (cons item (cons (rear-ptr deque) '()))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-triplet)
	   (set-rear-ptr! deque new-triplet)
	   deque)
	  (else
	   (set-next-ptr! (rear-ptr deque) new-triplet)
	   (set-rear-ptr! deque new-triplet)
	   deque))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else (set-front-ptr! deque (cddr (front-ptr deque)))
	      (set-prev-ptr! (front-ptr deque) '())
	      deque)))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else (set-rear-ptr! deque (cadr (rear-ptr deque)))
	      (set-next-ptr! (rear-ptr deque) '())
	      deque)))

(define (display-deque deque)
  (define (aux front-ptr)
    (cond ((null? front-ptr) '())
	  ((null? (cddr front-ptr))
	   (display (car front-ptr)))
	  (else
	   (display (car front-ptr)) (display " ")
	   (aux (cddr front-ptr)))))
  (display "(")
  (aux (front-ptr deque))
  (display ")"))



