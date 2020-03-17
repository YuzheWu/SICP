;;; SICP Exercise 2.74 by Yuzhe Wu, 29 Feb 2020


;; Part (a)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
(define (get-record employee-name file)
  (let ((division-name (division-of-file file)))
    (let ((get-record-division 
	   (get 'get-record division-name)))
      (if get-record-division
	  (apply get-record-division
		 (list employee-name 
		       (contents-of-file file)))
	  (error "No get-record method for division: GET-RECORD" 
		 division-name)))))

;; The individual divisions' files must include their division names along with 
;; the personnel records.


;; Part (b)

(define (get-salary record)
  (let ((division-name (division-of-record record)))
    (let ((get-salary-division
	   (get 'get-salary division-name)))
      (if get-salary-division
	  (get-salary-division (contents-of-record record))
	  (error "No get-salary method for division: GET-SALARY"
		 division-name)))))

;; Like individual files, individual records also need to include the division
;; information to facilitate dispatching of applicable methods.


;; Part (c)

(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      #f
      (let ((record (get-record employee-name (car division-files))))
	(if record
	    record
	    (find-employee-record employee-name (cdr division-files))))))


;; Part (d)

;; The new company must restructure its personnel file to be compatible with
;; the central system's information retrieval methods. In particular, the file
;; along with its records must include a division name and corresponding
;; division-specific methods for retrieving individual records and record 
;; details should be implemented and installed in the central lookup table.
