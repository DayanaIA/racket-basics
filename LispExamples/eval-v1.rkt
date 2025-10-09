#lang scheme

;EVAL
; representation: (+ x (* y 4)) like:

;1st way
'(+ x (* y 4))

; 2nd way
(list '+ 'x
      (list '* 'y '4))

; 3th way

(cons  '+ (cons 'x
                (cons (cons '* (cons 'y (cons 4 '()))) '())))

; quote operator for symbolic constants representation
'x
(quote x)
'(a (b c))
(quote(a (b c)))
(quote (+ x (* y 4)))

; =======================  Eval v1 =======================

(define (eval-1 exp)
  (if (number? exp) exp
      (apply-op (car exp)
                (eval-1 (cadr exp))
                (eval-1 (caddr exp)))))

(define (apply-op f x y)
  (if (equal? f '+)
      (+ x y)
      (if (equal? f '-) (- x y)
          (if (equal? f '/)
              (/ x y)
              (if (equal? f '*)
                  (* x y)
                  'erro)))))

; test eval 1
(eval-1 '(+ 2 (/ 6 3)))




































