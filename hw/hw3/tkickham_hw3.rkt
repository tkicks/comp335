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
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(and (s-exp-list? s)
          (eq? (s-exp->symbol (first (s-exp->list s))) 'begin))
     (beginC (map parse (rest (s-exp->list s))))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "syntax error in list")]))]
    [else (error 'parse "syntax error")]
    ))

; the interpreter takes the internal representation
; of the program, executes it, and returns the result
; [multC] will actually multiply the two values

(define (interp [a : ArithC] [env : Env]) : number
  (type-case ArithC a
    [numC (n) n]
    [idC (id) (lookup id env)]
    
    [beginC (l)
            (cond
             [(<= 2 (length l))                     ; b/c 3+ will have other operations to interp
              (begin                                ; so that it only takes the last value from two
                (interp (first l) env)              ; will be used when on the last item
                (interp (beginC (rest l)) env))]    ; to get to the last item
             [else
              (interp (first l) env)])]             ; if there's another operation in the list
    
    [declareC (id val)
              (let ([interpValue (interp val env)])            ; interp the value to get the type right
                (begin                                         ; get the final result
                  (add-env (bind id (box interpValue)) env)    ; add an environment and bind the boxed value to it
                  interpValue))]                               ; return the value
    
    [setC (id val)
          (let ([lookupValue (lookup id env)])                             ; create var to hold the value of the id
                             (begin                                        ; only take the final value
                               (add-env (bind id (box lookupValue)) env)   ; add an env to the boxed value bound to the id
                               lookupValue))]                              ; return the value
    
    [plusC (l r) (+ (interp l env) (interp r env))]
    ))

; eval will parse and interpret ArithC s-expressions
;(define (eval [input : s-expression]) : number
;  (interp (parse input))
;  )

; environment stuff
(define-type Binding
  [bind (name : symbol) (val : (boxof number))]     ; boxof number b/c all numbers boxed
  )
(define-type-alias Env (boxof (listof Binding)))
(define mt-env (box empty))
(define (add-env [b : Binding] [e : Env]) : Env
  (begin 
    (set-box! e (cons b (unbox e)))
    e))


; lookup function
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? (unbox env)) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first (unbox env))))
             (unbox (bind-val (first (unbox env))))]
            [else (lookup for (box (rest (unbox env))))])]))


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
;(test (eval '(+ 13 12)) 25)
; lower delimiter
(display "================================================================\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hw3
; upper delimiter
(display "================================================================\n")
(display "hw3\n")
(display "test for declareC doesn't pass")
;(test (parse '(five 5)) (declareC (idC five (numC 5))))
(display "================================================================\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;