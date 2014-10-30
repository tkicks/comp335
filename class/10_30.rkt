#lang plai-typed

;(define (makeinc [n : number]) : (number -> number)
;  (lambda ([y : number]) : number
;    (+ n y)))

;(define inc4 (makeinc 4))
;(define inc6 (makeinc 6))

(display "1.a. ==========================================\n")
;(test (inc4 6) 10)
;(test (inc4 1) 5)
;(test (inc6 -3) 3)
(display "commented out for 1.c.\n")

(display "\nb. ==========================================\n")
(display "the environment is 4 because it's what's being added to it\n")

(display "\nc. ==========================================\n")
(define makeinc (lambda ([n : number]) : (number -> number)
  (lambda ([y : number]) : number
    (+ n y))))

(define inc4 (makeinc 4))
(define inc6 (makeinc 6))

(test (inc4 6) 10)
(test (inc4 1) 5)
(test (inc6 -3) 3)

(display "\nd. ==========================================\n")
(define mylist (list 1 2 3 4 5))
(define (inc5 [n : number]) : number
  (+ n 5))
(test (map inc5 mylist) (list 6 7 8 9 10))

(map makeinc mylist)
(display "\n(map makeinc mylist) evaluates to a list of new incn functions where n is each element in the list\n")

(display "\ne. ==========================================\n")
(define (apply-with-one [f : (number -> number)]) : number
  (f 1))
(test (map apply-with-one (map makeinc mylist)) (list 2 3 4 5 6))
(display "It returns the list plus one\n")

(display "\n2.a. =========================================\n")
(define (greater-than [x : number] [y : listof number]) : listof number
  (> x y))
