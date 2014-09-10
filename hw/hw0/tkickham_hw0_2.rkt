#lang plai-typed

;set up Tree
(define-type Tree
  [node (n : number) (left : Tree) (right : Tree)]
  [leaf])

;combine lists together since cons can't
;passes in the left side of the tree (list1)* and the right side of the tree (list2)*
;returns a combined list of numbers
(define (combine [list1 : (listof number)] [list2 : (listof number)]) : (listof number)
  (cond
    ;if it's empty return the second list (has all sorted numbers)
    [(empty? list1) list2]
    ;otherwise,
    [else
     ;recursively call on
     (combine
      ;the rest of the list1 after the highest number was taken out
      ;(reversed so highest to lowest, rest of that eliminating highest, reversed for recursive success)
      (reverse (rest (reverse list1)))
      ;and list2 with the highest number of list 1 consed in
      ;list1 is still full at this point, so it reverses the list and takes the first (highest) number
      (cons (first (reverse list1)) list2))
     ])
  )

;base function to combine the two lists
(define (tree-to-list [t : Tree]) : (listof number)
  (cond
    [(leaf? t) empty]
    [else
     ;call combination function to put both lists together
     (combine
      ;pass in the left half of the tree to list1
      (tree-to-list (node-left t))
      ;pass in the value of the current node consed to the right side of the list to list2
      (cons (node-n t) (tree-to-list (node-right t)))
      )]
    )
  )

;test function call
(tree-to-list (node 5 (node 4 (node 3 (leaf) (leaf)) (leaf)) (node 7 (leaf) (node 9 (node 8 (leaf) (leaf)) (leaf)))))