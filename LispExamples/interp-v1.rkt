#lang plai-typed

; 1. Atomic data
;Booleans
true false #t #f

;Numbers
1 4.23 2/3 4+5i -3

;Strings
"a thing"

;Symbols: represents a name
'foo 'bar 'x 'something01!

;Characteres
#\a #\space

;1.1 Basic functions for atomic data

; not, and, or
; +, -, *, /
; <, >, =, etc (numéricos)
; string=?, char=?
; equal? — mesmo resultado
; eq? — mesma estrutura e resultado
; string-append, string-ref

;1.2 S-expressions: is the basic data structure
; cloud be an atom or a list of s-expressions
; a symbol is a s-expression

; frist = car list
(first (s-exp->list '(a b c)))

; second = (car (cdr list))
(second (s-exp->list '(a b c)))
(second (s-exp->list '(a (b c) d)))

; third = (car (cdr (cdr lst)))
(third (s-exp->list '(a (b c) d)))

#|==================================================================
 | Example define-type and type-case
 |==================================================================
 |#

; to test: (test val result)
(test (+ 1 2) 3)

; define-type
; type-case: to verify a type

(define-type Animal
   [Dog (name : string) (age : number)]
   [Cat (name : string) (lives : number)]
  )

(define fido (Dog "Fido" 4))
(define luna (Cat "Luna" 7))

(define (describe [animal : Animal]) : string
  (type-case Animal animal
    [Dog (name age) (string-append name " is a dog")]
    [Cat (name lives) (string-append name " is a cat")]
    ))

(describe fido)
(describe luna)











