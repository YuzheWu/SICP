;;; SICP Exercise 2.68 by Yuzhe Wu, 28 Feb 2020


(define (encode message tree) 
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree)))
	 (error "bad symbol: ENCODE-SYMBOL" symbol))
	((leaf? tree) '())
	((memq symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((memq symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))))
      