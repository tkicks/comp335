#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ArithC
 [numC (n : number)]
 [idC (id : symbol)]
 [plusC (left : ArithC) (right : ArithC)]
 [beginC (l : (listof ArithC))]
 [declareC (id : symbol) (val : ArithC)]
 [setC (id : symbol) (val : ArithC)])

; the parser takes in an s-expression 
; and returns the internal representation of the program
; the parser checks that the s-expression is valid
(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
        [(and (s-exp-list? s)
          (eq? (s-exp->symbol (first (s-exp->list s))) 'begin))
            (beginC (map parse (rest (s-exp->list s))))]
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "syntax error in list")]))]
     [else (error 'parse "syntax error")]
    ))

; the interpreter takes the internal representation
; of the program, executes it, and returns the result
; [multC] will actually multiply the two values

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) (box n)]
    [plusC (l r) (box (+ (unbox (interp l e)) (unbox (interp r e))))]
))

; eval will parse and interpret ArithC s-expressions
(define (eval [input : s-expression]) : number
  (interp (parse input))
)

(define-type-alias Env (boxof (listof Binding)))
(define mt-env (box empty))
(define (add-env [b : Binding] [e : Env]) : Env
 (begin 
 (set-box! e (cons b (unbox e)))
 e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
; upper delimiter
(display "================================================================\n")
(display "Problem 1\n")
; just a number
(test (parse '9) (numC 9))
; no numbers
(test/exn (parse '()) (error 'parse "empty"))
; no + symbol
(test/exn (parse '(1 2 4)) (error 'parse "no addition"))
; add two numbers together
(test (parse '(+ 1 2)) (plusC (numC 1) (numC 2)))
; add two sums together
(test (parse '(+ (+ 1 2) (+ 2 3)))
      (plusC (plusC (numC 1) (numC 2)) (plusC (numC 2) (numC 3))))
; lower delimiter
(display "================================================================\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
; upper delimiter
(display "================================================================\n")
(display "Problem 3\n")
; eval a plus two numbers
(test (eval '(+ 13 12)) 25)
; lower delimiter
(display "================================================================\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;