#lang nanopass

(require "lexer_tokens.rkt"
         "my_parser.rkt"
         parser-tools/yacc)

#|
       INTEGRANTES:
Aguilera Moreno Adrian.
Rojas Reyes Saul Adrian.
Aquino Chapa Armando Abraham.
Gutierrez Medina Sebastián Alejandro.
|#

;; OBS. Las estructuras las toma de my_parser.

; Función tomada de: https://stackoverflow.com/questions/13313815/scheme-convert-boolean-to-string
; Función que convierte de bool a string, en partícular.
(define (->string x)
  (call-with-output-string
   (lambda (out)
     (display x out))))

;; Función que nos regresa "int" o "bool" sea el caso:
(define (clasifica x)
  (if (equal? x 'INT)
      "int"
      "bool"))

;; Función que genera la regresentación como cadena de una lista, por ejemplo: a, b, c, ...
(define (cadena lista)
  (cadena/cc lista ""))
;; Auxiliar con recursión de cola para la funcióna anterior:
(define (cadena/cc lista acc)
  (if (empty? lista)
      acc
      (cadena/cc (rest lista) (string-append acc (->nanopass (first lista)) (if (empty? (rest lista))
                                                                                ""
                                                                                ", ")))
      ))

;; ->nanopass
(define (->nanopass e)
  (match  e
    [(num n)              (->string n)                                              ]
    [(bin-expr '+  e1 e2) (string-append "(+ "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '-  e1 e2) (string-append "(- "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '/  e1 e2) (string-append "(/ "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '*  e1 e2) (string-append "(* "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '%  e1 e2) (string-append "(% "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '== e1 e2) (string-append "(== "   (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '=  e1 e2) (string-append "(= "    (->nanopass e1) " " (if(cons? e2)
                                                                         (string-append "{"(cadena e2)"}")
                                                                         (->nanopass e2)) ")" )]
    [(bin-expr '!= e1 e2) (string-append "(!=  "  (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '<  e1 e2) (string-append "(< "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '>  e1 e2) (string-append "(< "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '<= e1 e2) (string-append "(<= "   (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '>= e1 e2) (string-append "(>= "   (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '&  e1 e2) (string-append "(& "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-expr '\| e1 e2) (string-append "(| "    (->nanopass e1) " " (->nanopass e2) ")" )]
    [(un-expr '- e)       (string-append "(-"     (->nanopass e)                      ")" )]
    [(un-expr '! e)       (string-append "(!"     (->nanopass e)                      ")" )]
    [(id e1)              (->string e1)        #| e1 ya es string |#                       ]
    [(bool e1)            (->string e1)                                                    ]
    [(main e1)            (string-append "(main (" (->nanopass e1)                     "))" )]
    [(arg e1 e2)          (string-append "("      (->nanopass e1) " " (clasifica e2)  ")" )]
    [(arreglo e1 e2 e3)   (string-append          (->nanopass e1) ":" (clasifica e2)
                                         "["      (if (empty? e3)
                                                      " "
                                                      (->nanopass e3)) "]"                )]
    [(my-if e1 e2 e3)     (string-append "(if "   (->nanopass e1) (->nanopass e2)
                                                  (->nanopass e3)                     ")" )]
    [(my-while e1 e2)     (string-append "(while "(->nanopass e1) " { "
                                                  (->nanopass e2) " } "               ")" )]
    [(length e1)          (string-append "(length "(->nanopass e1)                    ")" )]
    [(return e1)          (string-append "(return "(->nanopass e1)                    ")" )]
    [(call e1 e2)         (string-append "("      (->nanopass e1) (->nanopass e2)     ")" )]
    [(procedimiento e1 e2 e3 e4) (string-append "("(->nanopass e1) "(" (if (empty? e2)
                                                                           " "
                                                                           (->nanopass e2))
                                                ")" (->nanopass e4)                   ")" )]
    [(cons x y)           (string-append (->nanopass x) (->nanopass y))]
    ; TODO. agregar  (...) ? a : b 
    ))

;;; helpers
(define (symbol-in-list x lst) 
  (if (symbol? x) (foldl (lambda (y acc) (or acc (symbol=? x y)))
      #f
      lst)
    #f
))

(define indice 0)
(define (indicepp) (set! indice (add1 indice)) indice)
(define (nueva) (string->symbol (~a "var" (number->string (indicepp)))))

(define (debug s [identificador ""]) (printf "~a:'~a'\n" identificador s))
(define (hash-ref* t keys) (map (lambda (x) (hash-ref t x) ) keys))
(define (hash-set-zip t keys values) (map (lambda (x) (hash-set! t (first x) (second x))) (for/list ([i keys] [j values]) (list i j))))

(define (operand? o)  (symbol-in-list o '(+ / * - ! != == % <= >= < > & \| while if)) )
(define (type? t) (symbol-in-list t '(int bool)))
(define (main? m) (if (symbol? m) (symbol=? m 'main) #f))
(define (var? v) (if (symbol? v) (not (string-contains? (symbol->string v) " ")) #f))

(define-language practica
    (terminals
        (operand (o))
        (number (n))
        (boolean (b))
        (type (t))
        (var (s))
        (main (main))
    )
    (program (prog) 
    ((main block) method* ...  )
    (main block)
    (method* ... )
  )
  (bloques (block)
    { statement* ...}
  )
  (expresiones (exp)
    (s [(s* t*) ...] t block )
    n
    b
    s
    (run exp* ...)
  )
  (runners (run)
    o
    s
  )
  (methods (method)
    (s [(s* t*) ...] t block )
  )
  (statements (statement)
    exp
    (= s exp)
    (while exp block)
    (if exp statement1 statement2)
    (return exp)
  )
)

(define-parser parser-practica practica)

(define (variables expr t) 
  (define (update-table ex) (begin 
    (if (symbol? ex) (begin  (if (hash-has-key? t ex) #t (hash-set! t ex (nueva))))
      (if (list? ex) (map update-table ex)
        (if (not (or (boolean? ex) (number? ex))) 
          (variables ex t)
          #f
          )
      )
    )
  ))
  ;;; (debug expr)
  ;;; (printf "1\n")
  (nanopass-case (practica program) expr 
    [((,main ,block) ,method* ... ) (begin (update-table block) (update-table method*))]
    [(,method* ... ) (update-table method*)]
    [else t]
  )
  ;;; (printf "2\n")
  (nanopass-case (practica bloques) expr 
    [{ ,statement* ...} (update-table statement*)]
    [else t]
  )
  ;;; (printf "3\n")
  (nanopass-case (practica  expresiones) expr 
    [,n t]
    [,b t]
    [,s (update-table s)]
    [(,run ,exp* ... ) (update-table exp*)]
    [else t]
  )
  (nanopass-case (practica runners) expr
    [,o t]
    [,s (update-table s)] 
    [else t]
  )
  ;;; (printf "4\n")
  (nanopass-case (practica methods) expr 
    [(,s [(,s* ,t*) ...] ,t ,block ) (begin (update-table s) (update-table s*) (update-table block)) ]
    [else t]
  )
  ;;; (printf "5\n")+
  (nanopass-case (practica statements) expr 
    
    [(= ,s ,exp) (update-table s) ]
    [(while ,exp ,block) (update-table block)]
    [(if ,exp ,statement1 ,statement2) (begin (update-table statement1) (update-table statement2)) ]
    [(return ,exp) (update-table exp)]
    [,exp t]
    [else t]
  )
  ;;; (printf "6\n")
  t
)



(define-pass rename-vars : practica (ir table) -> practica () 
  (program : program (ir) -> program ()
    [((,main ,[block]) ,[method*] ...  ) `((,main ,block) ,method* ... )]  ;;;[(lambda (,x* ...) ,abody) #t]
    [(,[method*] ... ) `(,method*  )] 
    
  )
  (bloques : bloques (ir) -> bloques ()
    [{ ,[statement*] ...} `{ ,statement* ...}]
  )
  ( expresiones :  expresiones (ir) ->  expresiones ()
    [,n `,n]
    [,b `,b]
    [,s `,(hash-ref table s) ]
    [(,o ,[exp*] ... ) `(,o ,exp* ...)]
    [(,s ,[exp*] ...) `(,(hash-ref table s) ,exp* ...)]
  )
  (methods : methods (ir) -> methods ()
    [(,s [(,s* ,t*) ...] ,t ,[block] ) `(,(hash-ref table s) [(,(hash-ref* table s*) ,t*) ... ] ,t ,block )]
  )
  (statements : statements (ir) -> statements ()
    [,exp `,exp]
    [(= ,s ,[exp]) `(= ,(hash-ref table s) ,exp)]
    [(while ,[exp] ,[block]) `(while ,exp ,block)]
    [(if ,[exp] ,[statement1] ,[statement2]) `(if ,exp ,statement1 ,statement2) ]
    [(return ,[exp]) `(return ,exp)]
  )

)
 
 
(define (symbol-table-nanopass expr table) 
  (define (symbol-table ex) (symbol-table-nanopass ex table))
  (define (symbol-table* lst) (map (lambda (x) (symbol-table-nanopass x table)) lst))
  (nanopass-case (practica program) expr 
    [((,main ,block) ,method* ... ) (begin (symbol-table block) (symbol-table*  method*))]
    [(,method* ... ) (symbol-table* method*)]
    [else table]
  )
  (nanopass-case (practica bloques) expr 
    [{ ,statement* ...} (symbol-table* statement*)]
    [else table]
  )
  (nanopass-case (practica  expresiones) expr
    [,n table]
    [,b table]
    [,s table]
    [(,run ,exp* ... ) table]
    [else table]
  )
  (nanopass-case (practica runners) expr
    [,o table]
    [,s table] 
    [else table]
  )
  (nanopass-case (practica methods) expr 
    [(,s [(,s* ,t*) ...] ,t ,block ) (begin (hash-set! table s t) (hash-set-zip table s* t*)) ]
    [else table]
  )
  (nanopass-case (practica statements) expr 
    
    [(= ,s ,exp) table ]
    [(while ,exp ,block) (symbol-table block)]
    [(if ,exp ,statement1 ,statement2) (begin (symbol-table statement1) (symbol-table statement2)) ]
    [(return ,exp) table]
    [,exp table]
    [else table]
  )
  table
)

(define ejemplo '((main {(return (gcd a 2 3))}) (gcd [(a int) (b int) (x int)] bool {(while (!= a 0) {(if (< a b) (= b (- b a)) (= a (- a b)))}) (return b)})))
(define ejemploParseado (parser-practica ejemplo))
(symbol-table-nanopass (rename-vars ejemploParseado (variables ejemploParseado (make-hash))) (make-hash))

;;
;;



; Ejemplo de asignación simple:   (bin-expr '+ (num 4) (num 5)) 
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main(){a = b}")))))))

; Ejemplo de asignación múltiple:
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main(){a = b = c = d}")))))))

; Ejemplo con asignación:
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main() { a = true}")))))))

; Ejemplo con if/else (return):
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main() { if 5+3 > 4 {return 4+3} else { return 2+3} a = b = c =  4+5 }")))))))

; Ejemplo con solo if:
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main() { if 5+3 > 4 { return 4+3} else {2} }")))))))

; Ejemplo con length:
(parser-practica (read (open-input-string (->nanopass (jelly-parser (lex jelly-lex (open-input-string "main() {length(a)}")))))))

; Ejemplo con función definido explicitamente:
(define ejemplo1 '(main {(gcd [(a int) (b int) (x int)] bool {(while (!= a 0) {(if (< a b) (= b (- b a)) (= a (- a b)))}) (return b)})}))

; Llamada a función dentro de main:
(define ejemplo2 '(main {(return (gcd a 2 3))}))

(parser-practica ejemplo1)
(parser-practica ejemplo2)

;(->nanopass (jelly-parser (lex jelly-lex (open-input-string "main(){a:int[]={1,2,3,4,5} a:int = 3}"))))
;(->nanopass (jelly-parser (lex jelly-lex (open-input-string "main() { a = true}"))))
