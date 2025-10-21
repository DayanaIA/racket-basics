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
                                       (begin (display "elemento: ") (display par) (newline) (display "car conj: ") (display lista2) (not (equal? par (assoc (car par) lista2))))) ; assoc 
                                     lista1))))

; (assoc 1 '((1 'a) (2 'b))) ; (1 'a)
sub-lista-assoc
(sub-lista-assoc '((a 1) (b 2)) '((a 1) (b 2) (c 3)))

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

(define membro?0 (lambda (elemento conjunto = )
   (if (null? conjunto) #f
     (if (= elemento (car conjunto))
          #t
         (membro? elemento (cdr conjunto) = )))))


(define membro? (lambda (elemento conjunto = )
   (begin
     (display "-----------")
     (newline)
     (display conjunto)
     (newline)
    (if (null? conjunto) #f
     (if (= elemento (car conjunto)) #t
         (membro? elemento (cdr conjunto) = )))
    )
    ))

membro?
(membro? '() '() sub-lista-assoc)




