#lang scheme

(define mdc (lambda (m n)
              (let ( (resto (remainder m n)))
                (if (= resto 0)
                  n
                  (mdc n resto)))))

#|
 | mdc* v1
 | la llamada recursiva está dentro de mdc: (mdc ... (mdc*...)), No es recursión de cauda,
 | entonces
 | primero tiene que evaluar la llamada recursiva y después calcular mdc
 | cada cálculo de mdc queda pendiente en la pila
 |#
(define mdc*-inef (lambda (lista)
                    (if (= (car lista) 1 )
                        1
                        (if (null? (cdr lista))
                            (car lista)
                            (begin
                              (display "mdc") (newline)
                              (mdc (car lista) (mdc*-inef (cdr lista))))))))

#|
 | mdc* v2
 | usa recursión de cauda
 | aunque sea de cauda, aún se siguen calculando varios mdc's intermedios
 | no apila frames, un mismo frame se libera en cada llamada recursiva y 
 | se reusa en la siguiente llamada (cauda)
 |
 | "Aunque el código fuente usa recursión, el intérprete no crea nuevos frames,
 | sino que reutiliza el mismo, igual que lo haría con un bucle iterativo"
 |#
(define mdc*-inef2 (lambda (lista )
                      (if (= (car lista) 1)
                          1
                          (mdc*-aux2 (car lista) (cdr lista)))))

(define mdc*-aux2 (lambda (res-parcial lista)
                   (if (null? lista) res-parcial
                       (if (= (car lista) 1) 1
                           (begin
                             (display "mdc") (newline)
                             (mdc*-aux2 (mdc res-parcial (car lista)) (cdr lista))))))) 

(define mdc*-inef3 (lambda (lista)
                     (if (= (car lista) 1) 1
                         (mdc*-aux3 (car lista) (cdr lista)))))
(define mdc*-aux3 (lambda (valor-intermediario lista)
                    (if (null? lista) valor-intermediario
                        (if (= (car lista) 1) 1
                            (begin (write 'mdc)
                                   (newline)
                                   (let ((valor (mdc valor-intermediario (car lista))))
                                     (if (= valor 1)
                                         1
                                         (mdc*-aux3 valor (cdr lista)))))))))
                      
#|
 | Lo que queremos es que si un 1 está presente en la lista,
 | entonces, ningún mdc es ejecutado
 |
 | Usando continuaciones
 |#
(define id (lambda(x) x))

(define mdc-otimo* (lambda (lista)
                     (mdc*-aux-ot lista id)))

(define mdc*-aux-ot (lambda (lista resto-da-conta)
                    (if (= (car lista) 1) 1
                        (if (null? (cdr lista))
                            (resto-da-conta (car lista)) ; acabou a lista, calculamos
                            (mdc*-aux-ot (cdr lista)
                                         (lambda (n)
                                           (resto-da-conta (mdc (car lista) n)))
)))))


(define mdc*-efficient
  (lambda (lista)
    (letrec
        ((mdc*-aux-eff
          (lambda (lista continuacao)
            (if (= 1 (car lista))
                1
                (if (null? (cdr lista))
                    (continuacao (car lista))
                    (mdc*-aux-eff (cdr lista)
                                  (lambda (num)
                                    (continuacao (mdc (car lista)
                                                             num)))))))))
      (mdc*-aux-eff lista id))))

#| mdc-otimo* vs mdc*-efficient
 | 
 | mdc-otimo*:
               no es tail recursion (cada llamada crea una continuación,
               la llamada actual no se puede descartar porque la continuación necesita
               acceder a resto-da-conta y a (car lista))
               crea un nuevo frame por cada llamada
 | mdc*-efficient:
               es tail recursion ()
               reutiliza el mismo frame
 |#

(define mdc*-super-efficient
  (lambda (lista)
    (letrec
        ((mdc-aux
          (lambda (lista continuacao)
            (if (= 1 (car lista))
                1
                (if (null? (cdr lista))
                    (continuacao (car lista))
                    (mdc-aux (cdr lista)
                             (lambda (num)
                                   (if (= num 1) 1 ;se já achei 1 nao preciso do resto
                                       (begin (display 'mdc)
                                              (newline)
                                              (continuacao (mdc (car lista)
                                                                num)))))))))))
      (mdc-aux lista id))))


(define mdc-callcc* (lambda (lista)
   (call/cc (lambda (exit) ; argumento aqui é  o ponto de saída de mdc*
              (letrec ( (mdc*-aux 
                         (lambda (lista) 
                           (if (= (car lista) 1)
                               (exit 1)
                               (if (null? (cdr lista))
                                   (car lista)
                                   (mdc (car lista)
                                        (mdc*-aux (cdr lista))))))))
                (mdc*-aux lista))))))

(define mdc-s-inef (lambda (s-expr)
      (if (number? s-expr)
          s-expr
	  (if (null? (cdr s-expr))
	      (mdc-s-inef (car s-expr))
	      (mdc (mdc-s-inef (car s-expr))
                   (mdc-s-inef (cdr s-expr)))))))

(define mdc-s (lambda (s-expr)
  (letrec
      ((mdc-s-aux (lambda (s-expr continuacao)
                    (if (number? s-expr); se temos um numero, ele eh o resultado
                        (if (= s-expr 1)
                            1
                            (continuacao s-expr))
                        (if (null? (cdr s-expr)) ; lista de apenas um elemento?
                            (mdc-s-aux (car s-expr) continuacao) ; recursao simples
                            (mdc-s-aux (car s-expr)
                                       (lambda (resultado-car)
                                         (if (= resultado-car 1) 1
                                             (mdc-s-aux (cdr s-expr)
                                                        (lambda (resultado-cdr)
                                                          (if (= resultado-cdr 1) 1
                                                              (continuacao
                                                               (begin (display 'mdc) (newline)
                                                                      (mdc resultado-car
                                                                           resultado-cdr))))))))))))))
    (mdc-s-aux s-expr id))))

(define mdc-s-callcc (lambda (s-expr)
    (call/cc (lambda (exit)
               (letrec ( (mdc-s-aux (lambda (s-expr)
                                    (if (number? s-expr)
                                        (if (= s-expr 1)
                                            (exit 1)
                                            s-expr)
                                        (if (null? (cdr s-expr))
                                            (mdc-s-aux (car s-expr))
                                            (begin (display 'mdc)
                                                   (newline)
                                                   (mdc (mdc-s-aux (car s-expr))
                                                        (mdc-s-aux (cdr s-expr)))))))))
                         (mdc-s-aux s-expr))))))





; Test

(mdc*-inef '(20 48 32 1))

(mdc-s-inef '(1 (2) (3 4)))



