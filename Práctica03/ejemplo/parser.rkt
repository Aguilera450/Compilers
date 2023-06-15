;; Cambiar -- por //
;; es bool no boolean

; GRAMATICA DE MI LENGUAJE
; expr    -> expr + expr | ! expr | constante | id | ( expr )
; id      -> [a..z]+
; consante <- [0..9]+

; ARBOL DE SINTAXIS CONCRETA PARA "3 + var"
;         expr
;        / | \
;    expr  +  expr
;     |        |
;   const      id
;     |        |
;     3        var

; ARBOL DE SINTAXIS ABSTRACTA PARA 3 + (2 - 4)
;         +
;        / \
;       3   -
;          / \
;         2   4


#lang nanopass
(require "lexer.rkt"
         parser-tools/yacc)

;ESTRUCTURAS PARA LOS NODOS DEL ARBOL
(define-struct bin-exp (op arg1 arg2) #:transparent)
(define-struct un-exp (op arg1) #:transparent)
(define-struct numero (n) #:transparent)
(define-struct id (i) #:transparent)
(define-struct fun (args tipo cuerpo) #:transparent)

(define jelly-parser
  (parser
    [start expr]    ; simbolo inicial de mi gramatica
    [end EOF]       ; el parser se detiene cuando vea el token EOF
    [tokens contenedores vacios] ; tokens que va a reconocer el parser
    [error void]    ;procedimiento que tiene que seguir el parser cuando encuentre un error

    [precs  (nonassoc NOT) ;Precedencia y asociacion
            (right ADD)]

    [grammar                ;gramatica
     [expr
      [(id)             $1]
      [(constante)      $1]
      [(LP expr RP) $2]
      [(NOT expr)      (un-exp '! $2)]
      [(expr ADD expr) (bin-exp '+ $1 $3)]]
     [constante
        [(NUM) (numero $1)]]
     [id
        [(ID) (id $1)]]])) ;fin de la definicion del parser


;Probamos el parser:
(define (lex lexer input) (lambda () (lexer input)))
(jelly-parser (lex jelly-lexer (open-input-string "!(1 + x + 2)")))