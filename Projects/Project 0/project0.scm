;;; MIT 6.001 SCIP COURSE PROJECT 0
;;; Write-up by Yuzhe Wu, 25 Nov 2017



;;; ====== PART 2 ======


;; The following test cases explore the evaluation of simple
;; expressions.

-37
;value: -37

(* 3 4)
;value: 12

(> 10 9.7)
;value: #t

(- (if (> 3 4)
       7
       10)
   (/ 16 10))
;value: 42/5

(* (- 25 10)
   (+ 6 3))
;value: 135

(define double (lambda (x) (* 2 x)))
;value: double

(define c 4)
;value: c

c
;value: 4

(double c)
;value: 8

c
;value: 4

(double (double (+ c 5)))
;value: 36

(define times-2 double)
;value: times-2

(times-2 c)
;value: 8

(define d c)
;value: d

(= c d)
;value: #t

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d))))
;value: 4



;;; ====== PART 3 ======


;; This section introduces pretty printing for better read-
;; ability of code.

;; Bellow is an example produced by typing the following line:
;; (define abs C-j (lambda (a) C-j (if (> a 0) C-j a C-j (-a))))

(define abs
  (lambda (a)
    (if (> a 0)
	a
	(-a))))

;; NOTE: typing the [Enter] key followed by the [Tab] key pro-
;; duces the the same result as the [C-j] command.



;;; ====== PART 5 ======


;; Below are answers to some documentation questions.


;; 1. According to the Don't Panic manual, how do you invoke the
;;   stepper? What is the difference between the stepper and the
;;   debugger?
;;
;; ANSWER: Stepper could be invoked with the [M-s] command. It
;;   permits the user to trace the evaluation process of an
;;   expression, thus providing another way of debugging by
;;   approaching bugs from a different direction than does the
;;   debugger.


;; 2. According to the Guide to MIT Scheme, which of the words
;;   in the scheme expressions you evaluated in Part 2 above are
;;   "special forms"?
;;
;; ANSWER: if, define, cond


;; 6. What are the three methods for controlling complexity des-
;;   cribed in the learning objectives section of the course ob-
;;   jectives and outcomes? List one example from each category.
;;
;; ANSWER: 1) Building abstractions. E.g. higher-order procedures
;;   2) Controlling interactions. E.g. massage passing
;;   3) Designing new languages. E.g. embedded languages


;; 7. What does the MIT Scheme Reference Manual say about treat-
;;   ment of upper and lower case in expressions?
;;
;; ANSWER: Scheme is case-insensitive in naming identifiers. How-
;;   ever, within character and string constants, uppercase and
;;   lowercase forms are distinguished.


;; 8. What are the Edwin commands for creating a new file, and for
;;   saving a file? What is the difference between the *scheme*
;;   buffer and the *transcript* buffer?
;;
;; ANSWER: Create a new file: [C-x][C-f] followed by name of the
;;   file. Save a file: [C-x][C-s]. The *scheme* buffer is the de-
;;   fault buffer where the use could interact with the Scheme
;;   interpreter in REPL mode. The *transcript* buffer keeps a
;;   history of interactions with the Scheme interpreter. Unfort-
;;   unately, I do not find this buffer in the version of MIT
;;   Scheme I have on my Macintosh:(
