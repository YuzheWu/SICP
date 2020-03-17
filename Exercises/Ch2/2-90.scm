;;; SICP Exercise 2.90 by Yuzhe Wu, 17 Mar 2020


(define (install-sparse-term-list-package)
  ;; internal procedures
  ;; representation of sparse term list
  (define (adjoin-term term term-list) 
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (empty-termlist? term-list) (null? term-list)) 
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'sparse L))
  (put 'adjoin-term '(sparse) adjoin-term)
  (put 'the-empty-termlist '(sparse) 
       (lambda () (tag '())))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) 
       (lambda (L) (tag (cdr L))))
  (put 'empty-termlist? '(sparse) empty-termlist?))

(define (install-dense-term-list-package)
  ;; internal procedures
  ;; representation of dense term list
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term))
	   term-list)
	  ((empty-termlist? term-list)
	   (if (= (order term) 0)
	       (list (coeff term))
	       (adjoin-term term (list 0))))
	  ((= (order term) 
	      (1+ (order (first-term term-list))))
	   (cons (coeff term) term-list))
	  (else
	   (adjoin-term term (cons 0 term-list)))))
  (define (first-term term-list) 
    (make-term (-1+ (length term-list))
	  (car term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to rest of the system
  (define (tag L) (attach-tag 'dense L))
  (put 'adjoin-term '(dense) adjoin-term)
  (put 'the-empty-termlist '(dense)
       (lambda () (tag '())))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense)
       (lambda (L) (tag (cdr L))))
  (put 'empty-termlist? '(dense) empty-termlist?))


;; generic operations on term lists
(define (adjoin-term term term-list)
  (let ((proc (get 'adjoin-term 
		   (list (type-tag term-list)))))
    (if proc
	(proc term (contents term-list))
	(error "Bad term list type: ADJOIN-TERM" 
	       (type-tag term-list)))))
(define (first-term L) (apply-generic 'first-term L))
(define (rest-terms L) (apply-generic 'rest-terms L))
(define (empty-termlist? L) (apply-generic 'empty-termlist? L))


;; representation of term
(define (make-term order coeff) (list order coeff))
(define (order term) (car term)) 
(define (coeff term) (cadr term))


;; generic arithmetic on sparse/dense term lists
(define (add-terms L1 L2)
  (let ((type1 (type-tag L1))
	(type2 (type-tag L2)))
    (cond ((eq? type1 type2)
	   (add-terms-same-type L1 L2))
	  ((eq? type1 'sparse)
	   (add-terms (sparse->dense L1) L2))
	  ((eq? type2 'sparse)
	   (add-terms L1 (sparse->dense L2)))
	  (else
	   (error "Cannot add term lists of these types: ADD-TERMS"
		  (list type1 type2))))))
(define (add-terms-same-type L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1) 
	(else
	 (let ((t1 (first-term L1)) 
	       (t2 (first-term L2)))
	   (cond ((> (order t1) (order t2)) 
		  (adjoin-term
		   t1 (add-terms-same-type (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
		  (adjoin-term
                   t2 (add-terms-same-type L1 (rest-terms L2))))
		 (else 
		  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms-same-type (rest-terms L1)
					(rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (let ((type1 (type-tag L1))
	(type2 (type-tag L2)))
    (cond ((eq? type1 type2)
	   (if (empty-termlist? L1) 
	       ((get 'the-empty-termlist type1))
	       (add-terms (mul-term-by-all-terms (first-term L1) L2)
			  (mul-terms (rest-terms L1) L2))))
	  ((eq? type1 'dense)
	   (mul-terms (dense->sparse L1) L2))
	  ((eq? type2 'dense)
	   (mul-terms L1 (dense->sparse L2)))
	  (else "Cannot multiply term lists of these types: MUL-TERMS" 
		(list type1 type2)))))
(define (mul-term-by-all-terms t1 L) 
  (if (empty-termlist? L)
      ((get 'the-empty-termlist (type-tag L)))
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))
	  

;; conversion between dense and sparse representations
(define (sparse->dense L)
  (if (empty-termlist? L)
      ((get 'the-empty-termlist 'dense))
      (adjoin-term (first-term L)
		   (sparse->dense (rest-terms L)))))
(define (dense->sparse L)
  (if (empty-termlist? L)
      ((get 'the-empty-termlist 'sparse))
      (adjoin-term (first-term L)
		   (dense->sparse (rest-terms L)))))

