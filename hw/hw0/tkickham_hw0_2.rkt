#lang plai-typed

(define-type Tree
  [node (n : number) (left : Tree) (right : Tree)]
  [leaf])

(define (tree-to-list [t : Tree]) : (listof number)
  (cond
    [(leaf? t) '()]
    [else
     (cons
      (cons (node-n t) (tree-to-list (node-left t)))
      (cons (node-n t) (tree-to-list (node-right t)))
      )]
    )
  )

;test function call
(tree-to-list (node 5 (node 4 (node 3 (leaf) (leaf)) (leaf)) (node 7 (leaf) (node 9 (node 8 (leaf) (leaf)) (leaf)))))