#lang plai-typed

(define-type Tree
  [node (n : number) (left : Tree) (right : Tree)]
  [leaf])

(define (tree-to-list [t : Tree]) : (listof number)
  (cond
    [(leaf? t) '()]
    [else
     (cons (tree-to-list (node-left t)) (tree-to-list (node-right t)))
     ]
    )
  )