;;; SICP Exercise 3.8 by Yuzhe Wu, 19 Apr 2020


(define f 
  (let ((pdt 1))
    (lambda (x)
      (set! pdt (* pdt x))
      pdt)))