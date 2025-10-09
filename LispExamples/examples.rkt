#lang scheme

;(define (name arglist) expression)


; ======================= EXAMPLE 1 =======================
; create list from m to n
(define (createList m n)
  (if (< n m) '() (cons m (createList (+ 1 m) n))))

; test createList
createList
(createList 4 9)

; ======================= EXAMPLE 2 =======================
; func to validate if n divide m
; returns true or false
(define (divide? n m)
  (= (remainder n m) 0))

; test divide
divide?
(divide? 10 5)
(divide? 7 3)

; Given a number n and a list, return a new list wihtout the multiples of n
(define (removeMultiples n list)
  (if (null? list) list
      (if (divide? n (car list)) 
           (removeMultiples n (cdr list))
           (cons (car list) (removeMultiples n (cdr list))))))

; test removeMultiples
removeMultiples
(removeMultiples 2 (createList 1 9))

; ======================= EXAMPLE 3 =======================
; find the last element of a list

(define (findLast list)
  (if (null? (cdr list)) (car list)
             (findLast (cdr list))))
; test findLast
findLast
(findLast '(a b c d))

; ======================= EXAMPLE 4 =======================
; insert a new element in a sorted list
; (2 5)

(define (insert n list)
  (if (null? list)
      (cons n '())
      (if (< n (car list))
          (cons n list)
          (cons (car list) (insert n (cdr list))))))          
; test
(insert 3 '(1 2 5 7))


; ======================= EXAMPLE 5 =======================
; Sort Insertion
; Given (8 7 1 2), do
; ()
; (8)
; (7 8)
; (1 7 8)
; (1 2 7 8)

(define (insertion-sort lista)
  (if (null? lista) lista
      (insert (car lista)
              (insertion-sort (cdr lista)))))

; test
(insertion-sort '(8 7 1 2))

; ======================= EXAMPLE 6 =======================
; Association lists
; list of <key,value> pairs
; make association and manage association functions: mkassoc, assoc

;(assoc key list): receives a key and a list
;returns its value or '()
;((alan verde) (wander roxo))). Given wander, returns roxo

; first pair =  car list = (alan verde)
; first key =   caar list = alan
; first value = cadar list = verde
(define (assoc key list)
  (if (null? list) '()
      (if (eq? key (caar list))
          (cadar list)
          (assoc key (cdr list)))))
; test assoc
(assoc 'k2 ( list '(k1 v1) '(k2 v2)))

; ======================= EXAMPLE 7 =======================
; add a pair key,value or replace the value
(define (mkassoc chave valor lista)
  (if (null? lista) (list (list chave valor))
      (if (eq? chave (caar lista))
          (cons (list chave valor ) (cdr lista))
          (cons (car lista) (mkassoc chave valor (cdr lista))))))
; test
(mkassoc 'e 'b (list '(a b) '(c d) '(e f)))

; ======================= EXAMPLE 8 =======================
; Sets

; Empty set?
(define empty-set '()); here empty-set is a variable and has '() value
empty-set

(define (empty-set2) '()) ; here empty-set2 is a function which returns '()
empty-set2

; Is member?: list '(a b c d), b

(define (is-member? e set)
  (if (null? set)
      #f
      (if (eq? e (car set)) #t
          (is-member? e (cdr set)))))
;test
is-member?
(is-member? 'a  '(1 2 3 4 a))

; Add element
(define (add-element e set)
  (if (is-member? e set) set
      (cons e set)))
; test
add-element
(add-element 'e '(a b c))

; union of sets whitout repetition
(define (union set1 set2)
  (if (null? set1) set2
      (union (cdr set1) (add-element (car set1) set2))))
; test
union
(union '(1 2) '(1 3))

; ======================= EXAMPLE 9 =======================

(define (soma-sexpr l )
  (if (null? l) 0
      (if (number? l) l
         (+ (soma-sexpr (car l)) (soma-sexpr (cdr l))))))


(soma-sexpr ( list (+ 1 3) '(5)))
(soma-sexpr '(1 3 5))

; ======================= EXAMPLE 10 =======================

(define tmp 0) ;global variable
(define (soma-errada  l ) 
  (if (null? l) tmp
      (if (number? l) (+ tmp l)
          (begin (set! tmp (soma-errada (car l)))
                 (+ tmp (soma-errada (cdr l)))))))

soma-errada
(soma-errada '(1 4))

; Local variables
(define (soma-certa-aux l tmp)
  (if (null? l) tmp
      (if (number? l) (+ tmp l)
          (begin (set! tmp (soma-certa-aux (car l) tmp))
                 (soma-certa-aux (cdr l) tmp))))) 

(define (soma-certa l ) (soma-certa-aux l 0))

soma-certa
(soma-certa '(1 4))

















