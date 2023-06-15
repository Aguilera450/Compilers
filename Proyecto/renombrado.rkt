#lang nanopass
(provide (all-defined-out))
(require "variables.rkt"
         "nanopass-jelly.rkt")

(define cont 0)

; CREA UNA NUEVA VARIABLE CON OTRO NOMBRE, VAR-N
(define (nueva-variable)
  (let* ([str-num (number->string cont)]
         [str-sim (string-append "var_" str-num)])
    (set! cont (add1 cont))
    (string->symbol str-sim)))

; DADO UN CONJUNTO DE VARIABLES, LES CREA UNA NUEVA
(define (asignar-variables variables)
  (let ([tabla (make-hash)])
    (set-for-each variables
    (lambda (v) (hash-set! tabla v (nueva-variable))))
    tabla))

; RENOMBRA LAS VARIABLES DE UN PROGRAMA
(define-pass rename-var : jelly (ir) -> jelly ()


  (Programa : Programa (ir) -> Programa()
            [(programa ,m)    (let* ([variables  (vars-main m (mutable-set))]
                                     [tabla      (asignar-variables variables)] 
                                     [m1         (Main m tabla)])
                              (begin 
                              (set! cont 0)
                              `(programa ,m1)))]

            [(programa ,m (,cp* ...))   (let* ([variables (vars-main m (mutable-set))]
                                               [tabla (asignar-variables variables)]
                                               [m1    (Main m tabla)]
                                               [cp1   (map (lambda (cp) (Cuerpo cp)) cp*)])
                                        (begin
                                        (set! cont 0)
                                        `(programa ,m1 (,cp1 ...))))])

  (Main : Main (ir tabla) -> Main()
    [(main (,[Sentencia : s* tabla -> s1] ...))   `(main (,s1 ...))])

  (Cuerpo : Cuerpo (ir) -> Cuerpo ()
    [,met 
    
    (let* ([variables (vars-met ir (mutable-set))]
           [tabla-renombrado (asignar-variables variables)])

    `,(Metodo met tabla-renombrado))]
    
    [,func 
    (let* ([variables (vars-func ir (mutable-set))]
           [tabla-renombrado (asignar-variables variables)])

    `,(Funcion func tabla-renombrado))])

  (Funcion : Funcion (ir tabla) -> Funcion ()
    [(funcion ,i (,[Decla : d* tabla -> d1] ...) (,[Sentencia : s* tabla -> s1] ...))   `(funcion ,i (,d1 ...) (,s1 ...))])

  (Metodo : Metodo (ir tabla) -> Metodo ()
    [(metodo ,i (,[Decla : d* tabla -> d1] ...) ,t (,[Sentencia : s* tabla -> s1] ...))  `(metodo ,i (,d1 ...) ,t (,s1 ...))])

  (Sentencia : Sentencia (ir tabla) -> Sentencia ()
    [(if-s ,[Expr : e0 tabla -> e1] (,[s1*] ...) (,[s2*] ...)) `(if-s ,e1 (,s1* ...) (,s2* ...))]
    
    [(while ,[Expr : e0 tabla -> e1] (,[s1*] ...))             `(while ,e1 (,s1* ...))]


    [(= ,[Expr : e tabla -> e1] ,[s]) `(= ,e1 ,s)]

    [(return ,[Expr : e tabla -> e1])    `(return ,e1)]
    
    [,d  `,(Decla d tabla)]
    
    [,e  `,(Expr e tabla)])

  (Decla : Decla (ir tabla) -> Decla ()
    [(: ,i ,t)                    `(: ,(hash-ref tabla i) ,t)])

  (Expr : Expr (ir tabla) -> Expr () ;
    [,c                            `,c]
    
    [,i                            `,(hash-ref tabla i)]
    
    [(,op ,[e0])                   `(,op ,e0)]
    
    [(,op ,[e0] ,[e1])             `(,op ,e0 ,e1)]
    
    [(get-elem ,i ,[e])            `(get-elem ,(hash-ref tabla i) ,e)]

    [(if-c ,[e0] ,[e1] ,[e2])      `(if-c ,e0 ,e1 ,e2)]

    [(arr-estruc ,[e*] ...) `(arr-estruc ,e* ...)]
    
    [(,i (,[e*] ...))                     `(,i (,e* ...))]

    [(length ,[Expr : i tabla -> i1])    `(length ,i1)]

    ;[(println ,[Expr : i tabla -> i1])    `(println ,i1)]
    
    ;[(println , c)    `(println , c)]
    ))


(define (renombrar archivo)
  (let* ([entrada (aceptado? archivo)]
         [salida  (rename-var entrada)])
    salida))