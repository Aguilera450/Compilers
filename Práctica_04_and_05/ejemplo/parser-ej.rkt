#lang nanopass
(require "lexer-ej.rkt"
         parser-tools/yacc)
(provide (all-defined-out))

;ESTRUCTURAS PARA LOS NODOS DEL ARBOL
(define-struct bin-exp (op arg1 arg2) #:transparent)
(define-struct un-exp (op arg1) #:transparent)
(define-struct numero (n) #:transparent)
(define-struct id (i) #:transparent)
(define-struct fun (args tipo cuerpo) #:transparent)
(define-struct if-stn (guardia then else) #:transparent)
(define-struct call (id args) #:transparent)
(define-struct autoincr (id) #:transparent)
(define-struct programa (m met) #:transparent)
(define-struct main (instr) #:transparent)
(define-struct metodo (id args tr instr) #:transparent)
(define-struct decl (id t) #:transparent)
(define-struct return (e) #:transparent)


(define jelly-parser
  (parser
    [start programa]    ; simbolo inicial de mi gramatica
    [end EOF]       ; el parser se detiene cuando vea el token EOF
    [tokens contenedores vacios] ; tokens que va a reconocer el parser
    [error (lambda (tok-ok? tok-name tok-value) ;Mensaje de error simple
    (raise-syntax-error 'error "no fue posible procesar un token" (if tok-value tok-value tok-name)))]
    [grammar                ;; gramatica
     [programa
      [(main metodo) (programa $1 $2)]
      [(metodo) $1]]
     [metodo
      [(id LP decl-lst RP : type LK expr-lst RK) (metodo $1 $3 $6 $8)]]
     [expr-lst
      [(expr expr-lst) (list* $1 $2)]
      [(expr) (list $1)]]
     [decl-lst
      [(decl COMA decl-lst) (list* $1 $3)]
      [(decl) (list $1)]]
     [decl
      [(id : type) (decl $1 $3)]]
     [type
      [(INT) 'int]
      [(BOOLEAN) 'bool]]
     [main
      [(MAIN LK expr-lst RK) (main $3)]]
     [expr
      [(id)             $1]
      [(constante)      $1]
      [(LP expr RP) $2]
      [(NOT expr)      (un-exp '! $2)]
      [(expr + expr) (bin-exp '+ $1 $3)]
      [(expr > expr) (bin-exp '> $1 $3)]
      [(id ++) (autoincr $1)]
      [(my-if) $1]
      [(call) $1]
      [(RETURN expr) (return $2)]
      ]
     [call
        [(id LP expr-list RP) (call $1 $3)]]
     [expr-list
      [(expr COMA expr-list)  (list* $1 $3)]
      [(expr)            (list $1)]]
     [my-if
      [(IF LP expr RP expr ELSE expr) (if-stn $3 $5 $7)]
      [(IF expr LK expr-list RK) (if-stn $2 $4 void)]
      ]
     [constante
        [(NUM) (numero $1)]]
     [id
        [(ID) (id $1)]]])) ;fin de la definicion del parser


;Probamos el parser:
(define (lex lexer input) (lambda () (lexer input)))


(define (->nanopass e)
  (match  e
    [(programa m met) (string-append
                        "(programa "
                            (->nanopass m) " "
                            (->nanopass met)")")]
    [(main inst) (string-append
                        "(main ("
                            (->nanopass inst) "))")]
    [(if-stn g t e) (string-append
                        "(if-stn "
                            (->nanopass g) " "
                            (->nanopass t) " "
                            (->nanopass e) ")")]
    [(metodo idt args tr instr) (string-append "(metodo "
                                                (->nanopass idt) " "
                                                "[" (->nanopass args) "]" " "
                                                (->nanopass tr) " "
                                               "(" (->nanopass instr)"))")]
    [(decl i t) (string-append "("  (->nanopass i) " "(->nanopass t)")")]
    [(cons h t) (string-append (->nanopass h) (->nanopass t))]
    ['() ""]
    ['int "int" ]
    ['bool "bool" ]
    [(return i) (string-append "(return " (->nanopass i) ")")]
    [(autoincr i)
                    (string-append
                        "(= " (->nanopass i) "(+ 1 " (->nanopass i) "))")]
    [(id i) i]
    [(bin-exp '> e1 e2) (string-append "(> " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(numero n) (number->string n)]
    [(bin-exp '+ e1 e2) (string-append "(+ " (->nanopass e1) " " (->nanopass e2) ")" )]
    [else "a"]))

