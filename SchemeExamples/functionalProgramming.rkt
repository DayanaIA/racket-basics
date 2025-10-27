#lang scheme

; This file contains examples from de
; Programming Languages - An Interpreter-Based Approach book


; navegate into lists

(define list1 '(a b c d))
(car list1)
(cdr list1)
(cadr list1)  ; (car (cdr list))
(cddr list1)  ; (cdr (cdr list))
(caddr list1) ; (car (cdr (cdr list)))

; (define (f x1 ... xN) expr) ~ (define f (lambda (x1... xN) expr))
; ~: equivalente

(define (suma x y) (+ x y))
(suma 4 7)

(define sum (lambda (x y) (+ x y)))
(sum 4 7)

#| 
 | GENERAL SORT
 |#

(define general-sort (lambda (num1 num2 comparacao)
                 (if (comparacao num1 num2)
                     (list num1 num2)
                     (list num2 num1))))
(general-sort 7 5 <) ; (5 7)
(general-sort 7 5 >) ; (7 5)

; sort pairs
(define compara-pares (lambda (par1 par2)
                        (if (< (car par1) (car par2))
                            #t
                            (if (< (car par2) (car par1))
                                #f
                                (< (cadr par1) (cadr par2))))))
(general-sort '(4 5) '(4 3) compara-pares)


; lexicographic sort
(define ordem-lexicografica-numeros
  (lambda (lista1 lista2)
    (if (null? lista1)
        #t
        (if (null? lista2)
            #f
            (if (< (car lista1) (car lista2))
                #t
                (if (> (car lista1) (car lista2))
                    #f
                    (ordem-lexicografica-numeros (cdr lista1) (cdr lista2))))))))
(ordem-lexicografica-numeros '(1 2 3 4) '(1 2 3))
(ordem-lexicografica-numeros '(1 2 3 4) '(1 2 4 4))


; generic insert-sort
(define insert (lambda (less-than x lista)
                 (if (null? lista)
                     (list x) ; return x as a list
                     (if (less-than x (car lista))
                         (cons x lista)
                         (cons (car lista) (insert less-than x (cdr lista)))))))

(insert < 2 '())
(insert < 2 '(1))
(insert < 2 '(1 3))

(define insertion-sort (lambda (less-than list )
                         (if (null? list)
                             list
                             (insert less-than (car list)
                                     (insertion-sort less-than (cdr list))))))

(insertion-sort ordem-lexicografica-numeros '((4 5) (4 3)))

                
; invert order
(define inverte-ordem(lambda (compara)
                       (lambda (x y)
                         (compara y x))))

((inverte-ordem <)1 2) ; #f

(insertion-sort (inverte-ordem ordem-lexicografica-numeros) '((4 5) (4 3)))

; derivada en un punto
; f'(x) = (f(x+dx) - f(x))/dx

(define deriva (lambda (f dx)
                 (lambda (x)
                   (/ (- (f (+ x dx))
                         (f x))
                      dx))))

(define f-square (lambda (x) (* x x)))

((deriva f-square 0.0001) 2)

(define deriva-square (deriva f-square 0.0001))

(deriva-square 2)


#| 
 | FIND
 | dado um predicado pred
 | acha se algum elemento de uma lista satisfaz pred
 |#

(define less-than1 (lambda (x)
                  (if (< x 1)
                      #t
                      #f)))
(less-than1 2)
                 
(define find (lambda (pred lista)
               (if (null? lista)
                   #f
                   (if (pred (car lista))
                       #t
                       (find pred (cdr lista))))))

(find less-than1 '(2 1 3 4))

#| 
 | SORTS
 |#

(define compara-pares-de-pares (lambda (t1 t2)
                                 (if (compara-pares (car t1) (car t2) )
                                     #t
                                     (if (compara-pares (car t2) (car t1))
                                                        #f
                                                        (compara-pares (cadr t1) (cadr t2))))))

(compara-pares-de-pares '((1 4) (3 4)) '((1 3) (4 3)))


(define ordem-lexicografica-pares (lambda (<1 <2)
                                    (lambda (par1 par2)
                                      (if (<1 (car par1) (car par2))
                                          #t
                                          (if (<1 (car par2) (car par1))
                                              #f
                                              (<2 (cadr par1) (cadr par2)))))))

((ordem-lexicografica-pares < <) '(1 2) '(1 4)) ; #t
((ordem-lexicografica-pares > >) '(1 2) '(1 4)) ; #f


; SORTS - 2
(define ordem-estudantes (ordem-lexicografica-pares < >))

(general-sort '(10 8) '(9 10) ordem-estudantes)
(general-sort '(10 8) '(9 10) ordem-estudantes)

; SORTS - 3

(define n-esimo (lambda (n lista)
        (if (= n 0) (car lista)
            (n-esimo (- n 1)  (cdr lista)))))

(n-esimo 1 '(1 2 3)) ; 2, indices inician en 0

(define seleciona-2-colunas (lambda (num-col-1 num-col-2)
                              (lambda (l)
                                (list (n-esimo num-col-1 l) (n-esimo num-col-2 l)))))

((seleciona-2-colunas 2 3) '(7 5 3 1 4)) ; selecciona indices 2 y 3 = (3 1)

;SORTS - 4

(define compoe-2-1 (lambda (f g)
                     (lambda (x y)
                       (f (g x) (g y)))))

(define compara-colunas (lambda (num-col1 num-col2)
                          (compoe-2-1 (ordem-lexicografica-pares < <)
                                      (seleciona-2-colunas num-col1 num-col2))))


((compara-colunas 0 1) '(1 2 3 4) '(1 4 7 8)) ; compara '(1 2) '(1 4)#t
((compara-colunas 0 1) '(1 5 3 4) '(1 4 7 8)) ; compara '(1 5) '(1 4) #f

