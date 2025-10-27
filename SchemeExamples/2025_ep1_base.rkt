#lang plai-typed

#|
 | interpretador simples, sem variáveis ou funçõess
 |#

#| primeiro as expressões "primitivas", ou seja, diretamente interpretadas
 |#

(define-type ExprC
  [numC     (n : number)]
  [idC      (s : symbol)]
  [appC     (fun : ExprC) (arg : ExprC)]
  [setC     (s : symbol) (e : ExprC)]
  [letC     (s : symbol) (v : ExprC) (body : ExprC)]
  [plusC    (l : ExprC) (r : ExprC)]
  [multC    (l : ExprC) (r : ExprC)]
  [lamC     (arg : symbol) (body : ExprC)]
  [ifC      (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC    (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC     (pair : ExprC)]; Gets 1st element of a pair
  [cdrC     (pair : ExprC)]; Gets 2nd element of a pair
  ; adding new "primitive" operations
  [beginC   (e1 : ExprC) (e2 : ExprC)]
  [let*C    (s1 : symbol) (e1 : ExprC) (s2 : symbol) (e2 : ExprC) (body : ExprC)]
  [lambda2C (arg1 : symbol) (arg2 : symbol) (body : ExprC)]
  [app2C    (fun : ExprC) (arg1 : ExprC) (arg2 : ExprC)]
  [quoteC   (s : s-expression)]
  )

#| agora a linguagem aumentada pelo açúcar sintático
 | neste caso a operação de subtração e menus unário
 |#

(define-type ExprS
  [numS     (n : number)]
  [idS      (s : symbol)]
  [appS     (fun : ExprS) (arg : ExprS)]
  [setS     (s : symbol) (v : ExprS)]
  [letS     (s : symbol) (v : ExprS) (body : ExprS)]
  [plusS    (l : ExprS) (r : ExprS)]
  [bminusS  (l : ExprS) (r : ExprS)]
  [uminusS  (e : ExprS)]
  [multS    (l : ExprS) (r : ExprS)]
  [lamS     (arg : symbol) (body : ExprS)]
  [ifS      (c : ExprS) (y : ExprS) (n : ExprS)]
  [consS    (car : ExprS) (cdr : ExprS)]
  [carS     (pair : ExprS)]
  [cdrS     (pair : ExprS)]
  ; adding new operations
  [beginS   (e1 : ExprS) (e2 : ExprS)]
  [let*S    (s1 : symbol) (e1 : ExprS) (s2 : symbol) (e2 : ExprS) (body : ExprS)]
  [letrecS  (s : symbol) (e : ExprS) (body : ExprS)]
  [lambda2S (arg1 : symbol) (arg2 : symbol) (body : ExprS)]
  [app2S    (fun : ExprS) (arg1 : ExprS) (arg2 : ExprS)]
  [quoteS   (s : s-expression)]
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS     (n)        (numC n)]
    [idS      (s)        (idC s)]
    [appS     (fun arg)  (appC (desugar fun) (desugar arg))]
    [setS     (s e)      (setC s (desugar e))]
    [letS     (v e body) (letC v (desugar e) (desugar body))]
    [plusS    (l r)      (plusC (desugar l) (desugar r))]
    [bminusS  (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS  (e)        (multC (numC -1) (desugar e))]
    [multS    (l r)      (multC (desugar l) (desugar r))]
    [lamS     (a b)      (lamC a (desugar b))]
    [ifS      (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [consS    (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS     (c)        (carC (desugar c))]
    [cdrS     (c)        (cdrC (desugar c))]
    ; adding new operations
    [beginS   (e1 e2)            (beginC (desugar e1) (desugar e2))]
    [let*S    (s1 e1 s2 e2 body) (let*C s1 (desugar e1) s2 (desugar e2) (desugar body))]
    [letrecS  (s e body) (                                       ; (letrec ( (f e) exp )) ~ 
                         letC s (numC 0)                        ; (let ( f '())
                                 (beginC                        ;        (begin
                                    (setC s (desugar e))        ;              (set! f e)
                                    (desugar body)))]           ;               exp )))
    [lambda2S (a1 a2 b)        (lambda2C a1 a2 (desugar b))]
    [app2S    (fun arg1 arg2)  (app2C (desugar fun) (desugar arg1) (desugar arg2))]
    [quoteS   (s)              (quoteC s)]
    ))

; We need a new value for the box
(define-type Value
  [numV   (n : number)]
  [closV  (arg : symbol) (body : ExprC) (env : Env)]
  [consV  (car : Value) (cdr : Value)]
  [clos2V (arg1 : symbol) (arg2 : symbol) (body : ExprC) (env : Env)]
  [symV   (s : s-expression)]
 
  )


; Bindings associate symbol with Boxes
; we need this to be able to change the value of a binding, which is important
; to implement letrec.

(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])


; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store


; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Value); lookup returns the box, we need this to change the value later
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; achou!
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; vê no resto



; Primitive operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))


; Return type for the interpreter, Value


(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n env))]; we need to unbox the value in the environment before using it
    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (interp a env)))
            (type-case Value closure
              [closV (parameter body env)
                     (interp body (extend-env (bind parameter (box argvalue)) env))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]
    [setC (v exp) (begin (set-box! (lookup v env) (interp exp env))
                         (unbox (lookup v env)))]
    ; let - similar to appC
    [letC  (v e b)
           (interp b (extend-env (bind v (box (interp e env))) env))]
    ;I left plusC without error-checking
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    [lamC (a b) (closV a b env) ]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    ; Working with lists
    [consC (b1 b2) (let ( (car (interp b1 env))
                          (cdr (interp b2 env)))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       car]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       cdr]
                [else (error 'interp "cdr applied to non-cell")]
                )]
    ; adding new operations
    ; begin
    [beginC (e1 e2) (
                     let ([v1 (interp e1 env)])
                     (interp e2 env))]
    [let*C (s1 e1 s2 e2 body) (
                               let ([v1 (interp e1 env)]) ; 1. calcula e1 en un ambiente env, obtiene v1 y 
                                (let ([env1 (extend-env (bind s1 (box v1)) env)]) ;crea un ambiente env1 (extiende env),
                                  (let ([v2 (interp e2 env1)]) ; 2. calcula e2 en env1, obtiene v2 y 
                                    (let ([env2 (extend-env (bind s2 (box v2)) env1)]) ;crea un ambiente env2 (extiene env1)
                                      (interp body env2)))))] ; 3. calcula body en env2
    [lambda2C (a1 a2 b) (clos2V a1 a2 b env) ]
    [app2C (f a1 a2)
          (let ((closure (interp f env))
                (argvalue1 (interp a1 env))
                (argvalue2 (interp a2 env)))
            (type-case Value closure
              [clos2V (parameter1 parameter2 body env)
                     (interp body (extend-env (bind parameter1 (box argvalue1))
                                              (extend-env (bind parameter2 (box argvalue2)) env)))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]
    [quoteC (s) (symV s)]

    ))


; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(let) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(set!) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         ;adding new operations
         [(begin) (beginS (parse (second sl)) (parse (third sl)))]
         [(let*) (let*S (s-exp->symbol (list-ref sl 1))
                        (parse (list-ref sl 2))
                        (s-exp->symbol (list-ref sl 3))
                        (parse (list-ref sl 4))
                        (parse (list-ref sl 5)))]
         [(letrec) (letrecS (s-exp->symbol (second sl)) 
                        (parse (third sl))              
                        (parse (fourth sl)))]           
         [(lambda2) (lambda2S (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
         [(call2) (app2S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(quote) (let ([quoted (second sl)]) (quoteS quoted))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))

; Readloop

(define (readloop ) : void
  (let((s (read)))
    (cond
      [(s-exp-list? s) (let* ((sl (s-exp->list s))
                              (operator (s-exp->symbol (first sl))))
                         (case operator
                           [(@END) (void)]
                           [else  (let* ( (arits (parse s))
                                           (aritc (desugar arits))
                                           (value (interpS s)))
                                     (begin (display arits)
                                            (display "\n")
                                            (display aritc)
                                            (display "\n")
                                            (display value)
                                            (display "\n")
                                            (readloop)))]))]            

      [else (error 'parse "invalid input")])))

(readloop)


#| Examples
(interpS '(+ 10 (* 2 3)))

(interpS '(lambda x (car x)))
(interpS '(lambda2 x y (+ (car x) y)))

; parse s: [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))]
;                     (lamS (        'x              ) (parse (car x)))
;                     (lamS (        'x              ) (carS (parse x)))
;                     (lamS (        'x              ) (carS (idS x)))
(parse '(lambda x (car x)))

; desugar (parse s)
; [lamS    (a b)      (lamC a (desugar b))]
;                     (lamC 'x (desugar (carS (idS 'x))))
;                     (lamC 'x (         carC (desugar (idS 'x))))
;                     (lamC 'x (         carC (         idC 'x))))
(desugar (lamS 'x (carS (idS 'x))))

; interp (desugar (parse s))
; [lamC (a b) (closV a b env) ]
;             (closV 'x (carC (idC 'x)) '())
(interp (lamC 'x (carC (idC 'x))) mt-env)

(interpS '(let x 5 (begin (set! x 10) (+ x 5))))

(interpS '(let* x 2 y (+ x 3) (* x y)))

|#

