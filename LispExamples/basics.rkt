#lang scheme
;comments
;Define:
;1. (define variable expression)
(define n 3)
;2.  ( define (function arglist ) expression)
(define (extract str)
 (substring str 0 n))

(define (bake flavor)
  (printf "preheating oven...\n")
  (string-append flavor " pie"))

(define (nobake flavor)
  string-append flavor "jello")


; -----------------------------------------------------------------------
; -----------------------------------------------------------------------

; Symbolic expressions (s-expressions)
println "Symbolic expressions"
42 ; number
'red ; symbol
"helo" ;string
#t ;boolean
#f ; boolean

println "Lists"
; lists
'(a b c) ; (a b c)
(+ 2 3) ; 2+3 = 5
'(1 (2 3) 4) ; (1 (2 3) 4)
'(+ (* 4 2) (- 2 5)) ; (+ (* 4 2) (- 2 5))
(+ (* 4 2) (- 2 5)) ; (4*2) + (2-5) = 8 + -3

; Basic operations

println "Basic operations"
(> 2 5) (< 2 5) (= 4 4) (= 3 7) ; = just for numbers
(remainder 10 5) (remainder 5 10);resto
(and #t #f)
(or #f #f)
(not #f)

println "eq"
(eq? "a" "a") (eq? 'a 'a) (eq? 4 4)

println "number?"
(number? 5) (number? '5) (number? 'a) ; t t f

println "symbol?"
(symbol? 5) (symbol? '5) (symbol? 'a) (symbol? '(a b)) ; f f t f

println "list?"
(list? '(a b)) (list? (cons 'a '())) (list? (cons 'a 'b)) ; t t f

println "null?"
(null? '())

println "car"
(car '(a b c))

println "cdr"
(cdr '(a b c))

println "Constantes simbólicas"

; symbolic constants
(cons 'a '()) ; (a)
(cons 'a 'b) ; (a . b): a->b
(cons 'a (cons 'b '())) ; (a b): a->b->()

(define PI 3.1415)
PI

;Exemplo tamanho da lista
(define (tamanho list )
(if (null? list ) 0 (+ 1 (tamanho (cdr list)))))

; test tamanho
tamanho
(tamanho '(a b c))

; lists: (list arg1 arg2 … argn)
(list 'a 'b 'c 1 2 (+ 1 2))

; list of <key,value> pairs
println "list of <key,value> pairs"
(list '('alan 'verde) '('wander 'roxo))

; car list = (alan verde)
println "first pair = car list"
(car (list '(a b) '(c d)))

; first key = caar list = alan
println "first key = caar"
(caar (list '(a b) '(c d)))

; first value = cadr list = verde
println "first value cadar"
(cadar (list '(a b) '(c d)))


; Assigment set!
; (set! id expr):  evaluates expr and changes id (which must be bound in the enclosing environment) to the resulting value

(define greeted null) ; variable greeted = null = '(); global variable
 
(define (greet name)
  ; set! greeted (cons "Aramis" '())
  ; set! greeted '("Aramis")

  (set! greeted (cons name greeted))
  (string-append "Hello, " name))

(greet "Aramis") ; greeted = '("Aramis")
(greet "Porthos") ; greeted = '("Porthos" "Aramis")
(greet "Athos") ; greeted = '("Athos" "Porthos" "Aramis")

greeted



