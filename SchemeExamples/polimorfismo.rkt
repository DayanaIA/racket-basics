#lang scheme

#| 
 | ======================= POLIMORFISMO =======================
 |#

; Exemplo: polimorfismo 1- Listas de associação

; lista de associação são listas de pares (chave, valor)
; lista1 é sublista de lista2 se não conseguimos encontrar um elemento de lista1
; que não tenha o mesmo valor associado em listal e lista2
; lista1 = ((juan 10) (ana 20))
; lista2 = ((juan 10) (ana 20) (pedro 30) (luis 40))

(define find (lambda (pred lista)
               (if (null? lista)
                   #f
                   (if (pred (car lista))
                       #t
                       (find pred (cdr lista))))))
; Sub lista
(define sub-lista-assoc (lambda (lista1 lista2)
                          (not (find (lambda (par)
                                       (not (equal? par (assoc (car par) lista2)))) ; assoc 
                                     lista1))))

; (assoc 1 '((1 'a) (2 'b))) ; (1 'a)
sub-lista-assoc
(sub-lista-assoc '((a 1) (b 3)) '((a 1) (b 2) (c 3)))

; Igualdade lista
(define =lista-assoc (lambda (lista1 lista2)
                       (and (sub-lista-assoc lista1 lista2)
                            (sub-lista-assoc lista2 lista1))))
=lista-assoc
(=lista-assoc '((a 1) (b 2)) '((b 2) (a 1)))

#| 
 | Conjuntos
 |#

(define conj-vazio (lambda () '()))

(define membro?1 (lambda (elemento conjunto = )
   (if (null? conjunto) #f
     (if (= elemento (car conjunto)) #t
         (membro?1 elemento (cdr conjunto) = )))
    )
    )

membro?1
(membro?1 '() '() sub-lista-assoc)
; comparação simples
(membro?1 '(a 1) '((a 1) (b 2)) equal?)

; comparação com listas de associação
(membro?1 '((a 1)) '(((a 1)) ((b 2))) sub-lista-assoc)

(define adiciona1
  (lambda (elemento conjunto =)
    (if (membro?1 elemento conjunto =) conjunto
        (cons elemento conjunto))))


adiciona1
(adiciona1 'a '(b c d) equal?)
(adiciona1 '((a 1)) '(((a 1) (b 2))) sub-lista-assoc)


#| 
 | Conjuntos
 | Polimorfismo 2
 |#

(define conj-nulo (lambda(=) (list = '())))
(conj-nulo equal?) ; returns a list (#<procedure:equal?> ())

(define membro? (lambda (e conj)
                  (find ((curry (car conj)) e)
                        (cadr conj))))

(membro? 'a (list equal? '(a b c)))

(define adiciona (lambda (e conj)
                   (if (membro? e conj) conj
                       (list (car conj) (cons e (cadr conj))))))
(adiciona 1 (list equal? '(2 3 4)))

(define id (lambda (x) x))
(define combine (lambda (op f zero) 
         (lambda (lista) 
              (if (null? lista)
                  zero
                   (op  (f (car lista)) 
                          ((combine op f zero) (cdr lista)))))))

(define uniao (lambda (conj1 conj2)
                ((combine adiciona id conj2) conj1)))

(uniao (list equal? '(1 2 3)) (list equal? '(3 4 5)))
(uniao '(1 2 3) (list equal? '(3 4 5)))
                


(define lista1 '(1 2 3 4 5 6))
(define lista2 '(6 7 8 9))
(define conjunto1 (list equal? lista1))
(define conjunto2 (list equal? lista2))
(membro? 1 conjunto1)
(adiciona 7 conjunto1)
(uniao lista1 conjunto2)






