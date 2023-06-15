#lang nanopass
(provide (all-defined-out))
(require "lexer.rkt"
         parser-tools/yacc)

(define-struct programa    (main cuerpo) #:transparent)
(define-struct main        (sentencias) #:transparent)
(define-struct funcion     (nombre argumentos sentencias) #:transparent)
(define-struct metodo      (nombre argumentos tipo sentencias) #:transparent)
(define-struct while       (guardia sentencias) #:transparent)
(define-struct if-struct   (guardia then-sentencias else-sentencias) #:transparent)
(define-struct if-corto    (guardia then-sentencias else-sentencias) #:transparent)
(define-struct bool        (b) #:transparent)
(define-struct numero      (n) #:transparent)        
(define-struct id          (i) #:transparent)
(define-struct un-expr     (op e) #:transparent)
(define-struct bin-expr    (op e1 e2) #:transparent)
(define-struct asig        (id valor) #:transparent)
(define-struct asig-m      (ids valor) #:transparent)
(define-struct asig-t      (id tipo valor) #:transparent)
(define-struct decla       (id tipo) #:transparent)
(define-struct decla-m     (tipo ids) #:transparent)
(define-struct tipo        (t) #:transparent)
(define-struct llamada     (nombre argumentos) #:transparent)
(define-struct arr-estruc  (elementos) #:transparent)
(define-struct length      (nombre) #:transparent)
(define-struct return      (expresion) #:transparent)
(define-struct entrada-arr (id indice) #:transparent)

(define jelly-parser
  (parser
   [start programa]                        ; donde vamos a iniciar
   [end EOF]                               ; cuando parar, con el toker que representa el final del archivo
   [tokens contenedores vacios]            ; tokes x utilizar

  ;  [error void]
   [error (lambda (tok-ok? tok-name tok-value)
            (raise-syntax-error 'error "No fue posible procesar algun token" (if tok-value tok-value tok-name))
            )]

   [precs (nonassoc MAIN LP RP LCB RCB LC RC IF ELSE COMMA NOT)
          (right RETURN)
          (left OR)
          (left AND)
          (left EQ NEQ)
          (left LT GT GTEQ LTEQ)
          (left ADD SUBS)
          (left MULT DIV MOD)]

   [grammar

    [programa
     [(funcion-main)                  (programa $1 empty)]
     [(funcion-main cuerpo)           (programa $1 $2)]]

    [funcion-main
     [(MAIN LP RP LCB sentencias RCB) (main $5)]
     [(MAIN LP RP LCB RCB)            (main empty)]]

    [cuerpo
     [(funcion)           (list $1)]
     [(metodo)            (list $1)]
     [(funcion cuerpo)    (list* $1 $2)]
     [(metodo cuerpo)     (list* $1 $2)]]

    [funcion
     [(id LP declaraciones RP LCB sentencias RCB) (funcion $1 $3 $6)]
     [(id LP RP LCB sentencias RCB)               (funcion $1 empty $5)]]

    [metodo
     [(id LP declaraciones RP DOTS tipo LCB sentencias RCB) (metodo $1 $3 $6 $8)]
     [(id LP RP DOTS tipo LCB sentencias RCB)               (metodo $1 empty $5 $7)]]

    [sentencias
     [(sentencia)            (list $1)]
     [(sentencia sentencias) (list* $1 $2)]]

    [sentencia
     [(while)                $1]
     [(for)                  $1]
     [(if)                   $1]
     [(asignacion)           $1]
     [(asignacion-multiple)  $1]
     [(asignacion-tipo)      $1]
     [(asignacion-arreglo)   $1]
     [(declaracion)          $1]
     [(declaracion-multiple) $1]
     [(expresion)            $1]
     [(RETURN expresion) (return $2)]]

    [while
     ;[(WHILE LP expresion RP LCB sentencias RCB) (while $3 $6)]
     [(WHILE expresion LCB sentencias RCB)       (while $2 $4)]]

    [for
      [(FOR LP asignacion-tipo COMMA expresion COMMA expresion RP LCB sentencias RCB)  (list* $3 (while $5 (list $10 $7)))]]

    [if
     [(IF expresion LCB sentencias RCB)                                 (if-struct $2 $4 empty)]
     [(IF LP expresion RP LCB sentencias RCB ELSE LCB sentencias RCB)   (if-struct $3 $6 $10)]
     [(IF LP expresion RP sentencia ELSE sentencia)                     (if-struct $3 $5 $7)]]

    [asignacion
     [(id ASG expresion)               (asig $1 $3)]
     [(entrada-arreglo ASG expresion)  (asig $1 $3)]]
     
    [asignacion-multiple
     [(asignaciones expresion)  (asig-m $1 $2)]]

    [asignaciones
     [(id ASG)                  (list $1)]
     [(id ASG asignaciones) (list* $1 $3)]]

    [asignacion-tipo
     [(id DOTS tipo ASG expresion) (asig-t $1 $3 $5)]]

    [asignacion-arreglo
     [(id DOTS tipo ASG LCB expresiones RCB) (asig-t $1 $3 (arr-estruc $6))]]       ; CUIDAOO

    [declaracion
     [(id DOTS tipo) (decla $1 $3)]]

    [declaraciones
     [(declaracion)                     (list $1)]
     [(declaracion COMMA declaraciones) (list* $1 $3)]]

    [declaracion-multiple
     [(tipo identificadores) (decla-m $1 $2)]]

    [identificadores
     [(id)                       (list $1)]
     [(id COMMA identificadores) (list* $1 $3)]]

    [entrada-arreglo
     [(id LC expresion RC) (entrada-arr $1 $3)]]
    
    [expresiones
      [(expresion)                   (list $1)]
      [(expresion COMMA expresiones) (list* $1 $3)]]

    [id
     [(ID) (id $1)]]

    [expresion
     [(id)                                      $1]
     [(id LP expresiones RP) (llamada $1 $3)]
     [(id LP RP)             (llamada $1 empty)]
     [(LENGTH LP id RP)                  (length $3)]
     [(LP expresion RP)                         $2]
     [(expresion operador expresion)            (bin-expr $2 $1 $3)]
     [(operador-un expresion)                   (un-expr $1 $2)]
     [(expresion operador-un)                   (un-expr $1 $2)]
     [(expresion QSTN expresion DOTS expresion) (if-corto $1 $3 $5)]
     [(id LC expresion RC)                      (entrada-arr $1 $3)]
     [(NUM)                                     (numero $1)] 
     [(BOOLEAN)                                 (bool $1)]]

    [llamada
     [(id LP expresiones RP) (llamada $1 $3)]
     [(id LP RP)             (llamada $1 empty)]]
      
    [operador
     [(ADD)     '+]
     [(SUBS)    '-]
     [(DIV)     '/]
     [(MULT)    '*]
     [(MOD)     '%]
     [(LT)      '<]
     [(GT)      '>]
     [(AND)     'and]
     [(OR)      'or]
     [(LTEQ)    '<=]
     [(GTEQ)    '>=]
     [(EQ)      '==]
     [(NEQ)     '!=]
     [(ADDASG)  '+=]
     [(SUBSASG) '-=]]

    [operador-un
     [(AUTOINC) '++]
     [(AUTODEC) '--]
     [(NOT)     '!]]
      
    [tipo
      [(INT)     (tipo 'INT)]
      [(BOOL)    (tipo 'BOOL)]
      [(INT LC RC) (tipo 'ARR-INT)]
      [(INT LC NUM RC) (tipo 'ARR-INT)]
      [(BOOL LC RC) (tipo 'ARR-BOOL)]
      [(BOOL LC NUM RC) (tipo 'ARR-BOOL)]]


    [length 
     [(LENGTH LP id RP) (length $3)]]
     
    [return
     [(RETURN expresion) (return $2)]]
    
    ]))

(define (lex lexer input) (lambda () (lexer input)))
;(jelly-parser (lex jelly-lexer (open-input-string "main(){5 + 4}")))

(define (parsea archivo)
  (jelly-parser (lex jelly-lexer
                    (open-input-file archivo))))