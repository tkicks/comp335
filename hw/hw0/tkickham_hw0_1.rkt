#lang plai-typed

(define (insertion-sort 
         [x : (listof number)]) : (listof number)
  (cond
    [(empty? x) empty]
    [(< (first x) (first (rest x))) (insertion-sort (rest x))];(cons (first x) (insertion-sort(rest x)))]
    ;(cons (first x) (insertion-sort(rest x)))]
    
    ;good for all tests but sort
    ;[else (cons (first x) (insertion-sort(rest x)))]
    ;)
             
    [else (cons (first x) (insertion-sort (rest x)))])
    )


; Tests:
(test (insertion-sort (list 1 2 3)) (list 1 2 3))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)
