#lang plai-typed

(define (insertion-sort 
         [x : (listof number)]) : (listof number)
  (cond
    [(empty? x) empty]
    [else 
     (cond
<<<<<<< HEAD
       [(< first first(rest)) (reverse (x))]
=======
       [(< first rest[1]) (reverse (x))]
>>>>>>> 3461f577d1edfbd6a7b75ecdc66681a87a74547f
       ;(cons ((first x) number)
        ;     (insertion-sort (rest x))))
             
     )])
  )

; Tests:
(test (insertion-sort (list 1 2 3)) (list 1 2 3))
(test (insertion-sort (list 3 2 1)) (list 1 2 3))
(test (insertion-sort (list 1 3 1)) (list 1 1 3))
(test (insertion-sort (list 1)) (list 1))
(test (insertion-sort empty) empty)