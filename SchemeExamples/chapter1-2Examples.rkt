#lang scheme

; This file contains examples from de
; Programming Languages - An Interpreter-Based Approach book;
; Chapters 1 and 2

; create list from m to n
(define (createList m n)
  (if (< n m) '() (cons m (createList (+ 1 m) n))))

; soma
(define (soma-certa-aux l tmp)
  (if (null? l) tmp
      (if (number? l) (+ tmp l)
          (begin (set! tmp (soma-certa-aux (car l) tmp))
                 (soma-certa-aux (cdr l) tmp))))) 

(define (soma-certa l ) (soma-certa-aux l 0))


; sigma (m n) = m + (m + 1) + ... n
; sigma (1 4) = 1 + 2 + 3 + 4 = 10
(define (sigma-aux m n tmp)
  (if (> m n)
      tmp
      (begin (set! tmp (+ tmp m))
             (sigma-aux(+ m 1) n tmp))))

(define (sigma m n) (sigma-aux m n 0))

(sigma 3 7)

; SIGMA
(define (sigma2 m n)
  (if (> m n)
      0
      (+ m (sigma2 (+ m 1) n))))

(sigma2 1 10)

; exp(m n) = m^n (m, n >=0)
; exp(2 3) = 2 * 2 * 2 = 8

(define (exp-aux m n tmp)
  (if (< n 1)
      tmp
      (begin
        (set! tmp (* tmp m))
        (exp-aux m (- n 1) tmp))))

(define (exp m n) (exp-aux m n 1))

(exp -2 3)

; EXP
(define (exp2 m n)
  (if (= n 0)
      1
      (* m (exp m (- n 1)))))

(exp2 -2 4)


; log (m n) = el menor entero l, tal que m^(l+1) > n

(define (log-aux m n l)
  (if (> (exp2 m (+ l 1)) n)
      l
      (log-aux m n (+ l 1))))

(define (log m n) (log-aux m n 0))


(log 2 3) ;1
(log 2 4) ;2
(log 2 1) ;0

; Exercicios CAP 2

; count (x l) = cuentas el numero de ocurrecias de x a alto nivel de l
; (count 'a '(1 b a (c a))): x=a, l=(1 b a (c a)), count= 1


(define (count-aux x l c)
  (if (null? l)
      c
      ;(if (eq? x (car l)) (count-aux x (cdr l) (+ c 1)) (count-aux x (cdr l) c))
      (count-aux x (cdr l) (if (eq? x (car l)) (+ c 1) c))
      ))

(define (count2 x l) (count-aux x l 0))

count2
(count2 'a '(1 b a (a c a)))

; COUNT
(define (count x l)
  (if (null? l)
      0
      (+ (if (eq? x (car l)) 1 0) (count x (cdr l)))))

(count 'a '(a 1 b a (a c a)))

; COUNTALL
(define (countall x l)
  (cond
    [(null? l) 0]
    [(pair? (car l)) (+ (countall x (car l)) (countall x (cdr l)))]
    [else
     (+ (if (eq? x (car l)) 1 0) (countall x (cdr l)))]))

countall
(countall 'd '(a (b (c d))))






