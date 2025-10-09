#lang scheme

; This file contains examples from de
; Programming Languages - An Interpreter-Based Approach book;
; Chapter 4

; (lambda (x y ... z) e)
; replace x y ... z values in e

(define square (lambda (x) (* x x)))

square
(square 4)

; f = x^2 + y^2
(define f (lambda (x y) (+ (* x x) (* y y))))
(f 2 3) ; 2^2 + 3^2 = 4 + 9 = 13

(define sort2 (lambda (num1 num2 comparacao)
                 (if (comparacao num1 num2)
                     (list num1 num2)
                     (list num2 num1))))
(sort2 7 5 <) ; (5 7)
(sort2 7 5 >) ; (7 5)

; exemplo 2
(define compara-pares (lambda (par1 par2)
                        (if (< (car par1) (car par2))
                            #t
                            (if (< (car par2) (car par1))
                                #f
                                (< (cadr par1) (cadr par2))))))
(sort2 '(4 5) '(4 3) compara-pares)

;(compara-pares-de-pares '((1 2) (3 4)) '((1 3) (4 3)))

; exemplo 3
(define add (lambda (x) (lambda (y) (+ x y))))

((add 3) 4)

(define add1 (add 1))

(add1 3)




; MAPCAR:
; recebe uma função e uma lista
; retorna una lista com o resultado de aplicar la função a cada elemento da lista


(define mapcar (lambda (f l)
                 (if (null? l)
                     '()
                     (cons (f (car l))
                           (mapcar f (cdr l))))))


(mapcar number? '(1 2 3)) ; applies number? to each list's element
(mapcar add1 '(1 2 3)) ; applies add1 to each list's element


; CURRY

(define curry (lambda (f)
                (lambda (x)
                  (lambda (y) (f x y)))))

curry
(curry +)
((curry +) 4)
(((curry +) 4) 3)

(((curry mapcar) number?) '(1 2 3))

; MAPC
(define mapc (curry mapcar))

((mapc number?) '(1 2 a))

; ADD1*
(define add1* (mapc add1))

; ADD1**
(define add1** (mapc add1*))

; INCREMENT MATRIX
(define incrementa-matriz (mapc (mapc add1)))


;COMBINE
(define combine (lambda (f op zero)
                  (lambda (l)
                    (if (null? l)
                        zero 
                        (op (f (car l)) ((combine f op zero) (cdr l)))))))
                        

(define sum-squares (combine square + 0))

sum-squares
(sum-squares '(1 2 3))



; EXERCICIOS CAP 4

#| (a)
 | cdr* takes the cdr's of each element of a list of list
 |'(
 |  (a b c): cdr=(b c)
 |  (d e)  : cdr=(e)
 |  (f)    : cdr=()
 | )
 |
 | ( (b c) (e) () )
 |#

;(mapcar cdr '((a b c) (d e) (f)))
;(((curry mapcar) cdr) '((a b c) (d e) (f)))
;opcion 1
;(define (cdr* l) (mapcar cdr l))
;opcion 2
(define cdr* (mapc cdr))

(cdr* '((a b c) (d e) (f)))


#| (b)
 | max* finds the maximum of a list of non-negative integers
 | (4 3 10 5)
 | 10
 |#

(define id (lambda (x) x))
(define compare (lambda (x y) (if (> x y) x y)))
(define max* (combine id compare 0))

(max* '(2 5 4 5))

#| (c)
 | append: add an element to a list
 | e='a, l='(1 2)
 | '(a 1 2)
 |#

;(((curry cons) 'a) '(1 2))

(define append (curry cons)) ;(define mapc (curry mapcar))
((append '(a b)) '(1 2))
((append 'a) '(1 2))

#| (d)
 | addtoend (arg1 arg2):
 | adds arg1 (S-expression) as the last element of arg2 (a list):
 | arg1='a, arg2='(b c d)
 | '(b c d a)
 |#

(define (addtoend l1 l2)
  (if (null? l2)
      l1
      (cons (car l2) (addtoend (cdr l2) l1))))

(addtoend '(a) '(b c d))


