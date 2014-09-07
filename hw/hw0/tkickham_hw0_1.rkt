#lang plai-typed

(define (insertion-sort 
         [x : (listof number)]) : (listof number)
  (cond
    ;working parts
    ;good for 5
    [(empty? x) empty]
    ;good for 4
    [(empty? (rest x)) x]
    ;good for 1
    [(< (first x) (first (rest x))) (cons (first x) (insertion-sort (rest x)))]
    ;end working parts
    
    ;problem with recursion call and consing - error source for 2 and 3
    [(> (first x) (first (rest x))) (cons (first x) (insertion-sort (reverse (rest x))))]
    ;[(> (first x) (first (rest x))) (cons (first x) (insertion-sort (reverse (rest x))))]
    ))


; Tests:
(test (insertion-sort (list 1 2 3)) (list 1 2 3))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)
