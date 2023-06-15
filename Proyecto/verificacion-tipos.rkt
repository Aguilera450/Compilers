#lang nanopass
(provide (all-defined-out))
(require "nanopass-jelly.rkt"
          "tabla-simbolos.rkt"
          "renombrado.rkt")

(define (type-check ir tb)
  (nanopass-case (jelly Programa) ir
    [(programa ,[type-main : m tb -> tm]) ir]
    [(programa ,[type-main : m tb -> tm] (,[type-cuerpo : cp* tb -> *] ...)) ir]))

(define (type-main ir tb)
  (nanopass-case (jelly Main) ir
    [(main (,[type-sent : s* tb -> *] ... )) 
      'UNIT]))

(define (type-cuerpo ir tb)
  (nanopass-case (jelly Cuerpo) ir
    [,met (type-met met tb)]
    [,func (type-func func tb)]))

(define (type-func ir tb)
  (nanopass-case (jelly Funcion) ir
    [(funcion ,i (,d* ...) (,[type-sent : s* tb -> *] ... )) 
      'UNIT]))

(define (type-met ir tb)
  (nanopass-case (jelly Metodo) ir
    [(metodo ,i (,d* ...) ,t (,[type-sent : s* tb -> *] ... ,[type-sent : s1 tb -> ts1])) 
      (if (eq? ts1 t) 
        'UNIT
        (error "El tipo de regreso no coincide con la firma"))]))

(define (type-sent ir tb)
  (nanopass-case (jelly Sentencia) ir
    [(if-s ,[type-expr : e0 tb -> te0] (,[s1* -> ts1] ...) (,[s2* -> ts2] ...))
      (if (eq? te0 'BOOL)
          'UNIT
          (error "Guardia en IF no es una expresion BOOL"))]

    [(while ,[type-expr : e0 tb -> te0] (,[s1* -> ts1] ...))
      (if (eq? te0 'BOOL)
          'UNIT
          (error "Guardia en WHILE no es una expresion BOOL"))]

    [(= ,[type-expr : e tb -> te] ,[s -> ts]) 
      (if (or (eq? te ts) (eq? ts 'UNIT))
      'UNIT 
      (error "Los tipos no corresponden en la asignacion"))]
    
    [(return ,e) (type-expr e tb)]

    [,d 'UNIT]

    [,e (type-expr e tb)]))


(define (all-eq lst)
  (if (or (null? lst) (eq? (length lst) 1))
      #t
      (let* 
        ([fst   (first lst)]
         [resto (rest lst)])
        (foldr (lambda (x y) (and (eq? fst x) y)) #t resto))))



(define (type-expr ir tb)
  (nanopass-case (jelly Expr) ir 
  
    [(length ,[type-expr : i tb -> ti])  
      (if (memq ti '(ARR-INT ARR-BOOL)) 
        'INT 
        (error "Argumento de length no es de tipo arreglo"))]

    ;[(println ,[type-expr : i tb -> ti])  
      ;(if (memq ti '(INT BOOL STR)) 
       ;'UNIT
        ;(error "Argumento de println no es de tipo int, bool o string"))]

    ;[(println , c) 'UNIT]

    ; LLAMADA 
    ; SUMA => (INT INT), INT
    ; E*  => te1 te2
    [(,[type-expr : i tb -> firma] (,[e* -> te*] ...)) ;(sort . 'ARR-INT)
      (let*
          ([args (if (list? firma) firma (car firma))]) 
        (if (andmap eq? args te*) 
            (begin 
            (if (list? firma) 'UNIT (cdr firma))) 
            (error "Los tipos de los argumentos no corresponden a la firma de la funcion/metodo")))]

    [(arr-estruc ,[e* -> te*] ...) 
      (if (all-eq te*)
          (if(eq? (first te*) 'INT) 'ARR-INT 'ARR-BOOL)
          (error "Los elementos del arreglo no tienen el mismo tipo."))]

    [(if-c ,[e0 -> te0] ,[e1 -> te1] ,[e2 -> te2]) 
      (if (and (eq? te0 'BOOL) (eq? te1 te2))
          te1
          (error "Guardia no booleana o tipos de regreso diferentes en operador ternario"))]

    [(get-elem ,[type-expr : i tb -> ti] ,[e1 -> te1]) 
     
     (let ([tipo-regreso (if (eq? ti 'ARR-INT) 'INT 'BOOL)])
       (if (eq? te1 'INT) tipo-regreso (error "Indice no entero")))]

    [(,op ,[e0 -> te0] ,[e1 -> te1])
     (match op
       [(or '== '!= 'or 'and) 
          (if (eq? te0 te1) 'BOOL (error "Expresiones incomparables en operadores booleanos"))]
       [(or '> '< '<= '=> '== '!=) (if (eq? te0 te1) 'BOOL (error "Expresiones incomparables en operaciones de comparacion"))]
       [(or '+ '- '/ '% '*) 
          (if (eq? te0 te1) 
            'INT 
            (error "Expresiones incomparables en operaciones aritmeticas"))])]

    [(,op ,[e0 -> te0])
      (match op
        [(or '!) (if (eq? te0 'BOOL) 'BOOL (error "Expresiones incomparables en negacion"))]
        [(or '-) (if (eq? te0 'INT) 'INT (error "Expresiones incomparables en negativos"))])]

    [,c (if (number? c) 'INT 'BOOL)] ;(if (my-boolean? c) 'BOOL 'STR))]
    [,i (hash-ref tb i)]
    
    ))

(define (verificar-tipos archivo)
  (let* ([entrada (aceptado? archivo)]
         [e-renombrada  (rename-var entrada)]
         [tabla (tipos-programa e-renombrada (make-hash))])
    ;(println tabla)
    ;(println renombrado)
    (type-check e-renombrada tabla)))
    
