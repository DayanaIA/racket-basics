#lang plai-typed

#|
 | interpretador simples, com variáveis e funções
 |#

; define "primitive" expressions
(define-type ExprC
  [numC   (n : number)]
  [plusC  (l : ExprC) (r : ExprC)]
  [multC  (l : ExprC) (r : ExprC)]
  [gtrC   (l : ExprC) (r : ExprC)]
  [andC   (l : ExprC) (r : ExprC)]
  [orC    (l : ExprC) (r : ExprC)]
  [ifC    (cond : ExprC) (y : ExprC) (n : ExprC)]
  ; adding new primitive operations
  [divC   (l : ExprC) (r : ExprC)]
  [xorC   (l : ExprC) (r : ExprC)]
  [equalC (l : ExprC) (r : ExprC)]
  [notC   (e : ExprC)]
  )

; now, a language augmented by syntactic sugar
(define-type ExprS
  [numS    (n : number)]
  [plusS   (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [grtS    (l : ExprS) (r : ExprS)]
  [andS    (l : ExprS) (r : ExprS)]
  [orS     (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  ; adding new operations
  [minusS  (l : ExprS) (r : ExprS)] ; minus as syntactic sugar
  [divS    (l : ExprS) (r : ExprS)]
  [xorS    (l : ExprS) (r : ExprS)]
  [equalS  (l : ExprS) (r : ExprS)]
  [lessS   (l : ExprS) (r : ExprS)] ; less as syntactic sugar
  [notS    (e : ExprS)]
  )

;  ExprS  --desugar-->  ExprC
; (- a b) -----------> (+ a (* -1 b))
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [grtS    (l r)      (gtrC (desugar l) (desugar r))]
    [andS    (l r)      (andC (desugar l) (desugar r))] 
    [orS     (l r)      (orC (desugar l) (desugar r))] 
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    ; adding new operations
    [minusS  (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [divS    (l r)      (divC (desugar l) (desugar r))]
    [xorS    (l r)      (xorC (desugar l) (desugar r))]
    [equalS  (l r)      (equalC (desugar l) (desugar r))]
    [lessS   (l r)      (gtrC (desugar r) (desugar l))]
    [notS    (e)        (notC (desugar e))]
    ))

; define interpreter for primitives
(define (interp [a : ExprC] ) : number
  (type-case ExprC a
    [numC   (n)      n]
    [plusC  (l r)   (+ (interp l) (interp r))]
    [multC  (l r)   (* (interp l) (interp r))]
    [gtrC   (l r)   (if (> (interp l) (interp r)) 1 0)]
    [andC   (l r)   (if (= (interp l) 0) 0 (interp r))]
    [orC    (l r)   (if (= (interp l) 1) 1 (interp r))]
    [ifC    (c s n) (if (zero? (interp c)) (interp n) (interp s))]
    ; adding new primitive operations
    [divC   (l r)   (/ (interp l) (interp r))]
    [xorC   (l r)   (if (= (interp l) (interp r)) 0 1)]
    [equalC (l r)   (if (= (interp l) (interp r)) 1 0)]
    [notC   (e)     (if (= (interp e) 0) 1 0)]
   ))


; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+)     (plusS   (parse (second sl)) (parse (third sl)))]
         [(*)     (multS   (parse (second sl)) (parse (third sl)))]
         [(~)     (uminusS (parse (second sl)))]
         [(>)     (grtS    (parse (second sl)) (parse (third sl)))]
         [(and)   (andS    (parse (second sl)) (parse (third sl)))]
         [(or)    (orS     (parse (second sl)) (parse (third sl)))]
         [(if)    (ifS     (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         ; adding new operations
         [(-)     (minusS  (parse (second sl)) (parse (third sl)))]
         [(/)     (divS    (parse (second sl)) (parse (third sl)))]
         [(xor)   (xorS    (parse (second sl)) (parse (third sl)))]
         [(=) (equalS  (parse (second sl)) (parse (third sl)))]
         [(<)     (lessS   (parse (second sl)) (parse (third sl)))]
         [(not)   (notS    (parse (second sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))
; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s))))



#|========================================================================================
 |                                  TESTS
 ========================================================================================|#

; ==== Testing parse ====
(parse '(not (= 5 6)))
(parse '(- 5 6))
(parse '(~ 4))

;==== Testing desugar ====
(desugar (parse '(- 5 6)))
(desugar (parse '(< 5 6)))

; ==== Testing operations ====
(interpS '(= 5 6))
(interpS '(and (< 5 6) (> 10 9)))
(interpS '(not (= 5 6)))

(test (interpS '(+ (* 10 2) (~ 4))) 16)

; Minus "-"
(test (interpS '(- (* 10 2) (~ 4))) 24)

; Division "/"
(test (interpS '(/ (* 10 2) (- 4 2))) 10)

; XOR "xor"
#|
 l | r |
 ----------
 0 | 0 | 0
 0 | 1 | 1
 1 | 0 | 1
 1 | 1 | 0
 |#
(test (interpS '(xor (* 1 0) (* 1 0))) 0)
(test (interpS '(xor (* 1 0) (* 1 1))) 1)
(test (interpS '(xor (* 1 1) (* 1 0))) 1)
(test (interpS '(xor (* 1 1) (* 1 1))) 0)

; Equal "="
(test (interpS '(= (* 1 1) (* 1 0))) 0)
(test (interpS '(= (* 1 1) (* 1 1))) 1)

; Less "<"
(test (interpS '(< (* 1 2) (* 1 1))) 0)
(test (interpS '(< (* 1 1) (* 1 2))) 1)

; Not "not"
(test (interpS '(not (* 1 2))) 0)
(test (interpS '(not (* 1 0))) 1)

(test (interpS '(xor (= 5 5) (> 2 3))) 1)
(test (interpS '(xor (not (+ 2 1)) (> 2 3))) 0)
;               (xor (not (  3  )) (  0  ))
;               (xor (     0     ) (  0  )) = 0

