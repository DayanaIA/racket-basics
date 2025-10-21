#lang plai-typed

; Variáveis próprias

(define init-rand (lambda (valor-inicial)
                    (lambda () (begin
                                 (set! valor-inicial
                                       (remainder (+ (* valor-inicial 9) 5) 1024))
                                 valor-inicial))))

(define rand (init-rand 1))

; LET
; (let ( (x1 e1) (x2 e2)...(xN eN)) exp)
; primeiro calcula os valores (e1, e2, ...) no ambiente local
; obtendo v1, v2, ...
; logo, calcula expr no ambente
; rho{x1->v1,...,xN->vN} 
; ((lambda (x1...xN) exp) e1 ... eN)

; LET*
; (let* ((x1 e1) (x2 e2)...(xN eN)) exp)
; calcula e1 em rho obtendo v1, cria rho1 = rho{x1->v1}
; calcula e2 em rho1 obtendo v2, cria rho2 = rho1{x2->v2}...
;((lambda(x1)((lambda (x2)(...(lambda (xN) exp)eN)...)e2)e1)

;((lambda (x) (let ((x 3)(y x)) (+ x y))) 4) ; evaluates to 7
;((lambda (x) (let* ((x 3)(y x)) (+ x y))) 4)  ; evaluates to 6

; LETREC
; utilizado para definir funções recursivas localmente
; (letrec ((f e) exp)) ~ (let ((f '())
;                              (begin (set! f e)
;                                     exp)))

(letrec ((fact
          (lambda (n)
            (if (= n 0)
                1
                (* n (fact (- n 1)))))))
  (fact 5))

(letrec ((countdown
          (lambda (n)
            (if (= n 0)
                'done
                (countdown (- n 1))))))
  (countdown 3))

