;;; SICP Exercise 2.69 by Yuzhe Wu, 28 Feb 2020



(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-tree-set)
  (if (= (length leaf-tree-set) 1)
      (car leaf-tree-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-tree-set)
				  (cadr leaf-tree-set))
		  (cddr leaf-tree-set)))))
