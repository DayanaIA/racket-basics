#lang scheme

(define find (lambda (pred lista)
               (if (null? lista)
                   #f
                   (if (pred (car lista))
                       #t
                       (find pred (cdr lista))))))

#| 
 | CONJUNTOS - SETS
 |#

; Conjunto vazio
(define conj-vazio (lambda () '()))
conj-vazio
(conj-vazio)

; member?
(define member-v1 (lambda (e conj)
                  (find (lambda (x) (eq? x e)) conj)))
(member-v1 2 '(1 4 3))

(define member? (lambda (e conj)
                  (find ((curry eq?)e) conj)))
member?
(member? '1 '(1 2 3))

; adiciona
(define adiciona-elemento (lambda (e conj)
                            (if (member? e conj)
                                conj
                                (cons e conj))))
adiciona-elemento
(adiciona-elemento 'a '(b))

; união
(define combine (lambda (op f zero) 
         (lambda (lista) 
              (if (null? lista)
                  zero
                   (op  (f (car lista)) 
                          ((combine op f zero) (cdr lista)))))))

(define id (lambda (x) x))

(define uniao (lambda (conj1 conj2)
                ((combine adiciona-elemento id conj2) conj1)))

uniao
(uniao '(1 2 3) '(3 4))


























