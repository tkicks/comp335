#lang plai-typed

(define (insertion-sort 
         [x : (listof number)]) : (listof number)
  (cond
    ;if the list is empty return empty
    [(empty? x) empty]
    
    ;if the rest of the list is empty return the current number
    [(empty? (rest x)) x]
    
    ;if the first number is less than the first number in the rest of the list
    ;leave it at the front of the list and sort the rest
    [(< (first x) (first (rest x))) (cons (first x) (insertion-sort (rest x)))]
    
    ;if the first number is greater than the first number in the rest of the list
    [(> (first x) (first (rest x)))
     (cons
      (first (insertion-sort (rest x)))     ;first number of the rest of the list recursively until it's the lowest value
      (rest (insertion-sort (reverse x)))   ;the rest of the list reversed and recursively until it is sorted
      )]
    ))


; Tests:
(test (insertion-sort (list 1 2 3)) (list 1 2 3))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)
