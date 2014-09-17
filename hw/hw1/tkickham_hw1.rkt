#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ArithC
  [numC (n : number)]
  [plusC (left : ArithC) (right : ArithC)]
  [multC (left : ArithC) (right : ArithC)]
  )

; remember the REPL
; eval has two parts, the parser and the interpreter
; (print (eval (read))) --> 
;     (print (interpret (parse (read))))

; the parser takes in an s-expression 
; and returns the internal representation of the program
; the parser checks that the s-expression is valid
(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "syntax error in list")]))]
     [else (error 'parse "syntax error")]
    ))

; the interpreter takes the internal representation
; of the program, executes it, and returns the result
; [multC] will actually multiply the two values

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
; multiply two numbers
(test (parse '(* 1 2)) (multC (numC 1) (numC 2)))
; multiply the product of two equations
(test (parse '(* (* 3 7) (* 2 9))) (multC (multC (numC 3) (numC 7)) (multC (numC 2) (numC 9))))
; multiply two sums together
(test (parse '(* (+ 12 17) (+ 1 0))) (multC (plusC (numC 12) (numC 17)) (plusC (numC 1) (numC 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;