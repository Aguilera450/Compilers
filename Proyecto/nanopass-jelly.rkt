#lang nanopass
(provide (all-defined-out))
(require "parser.rkt"
         "lexer.rkt")

(define (->nanopass e)
  (match  e
    ['() ""]
    [(cons x xs)
     (string-append ""
                    (->nanopass x) " "
                    (->nanopass xs) "")]

    [(programa main cuerpo)
     (string-append "(programa "
                    (->nanopass main) " "
                    "( "(->nanopass cuerpo) "))")]

    [(main sentencias)
     (string-append "(main (  "
                    (->nanopass sentencias) "))")]

    [(funcion nombre argumentos sentencias)
     (string-append "(funcion " (->nanopass nombre) " "
                    "( "(->nanopass argumentos) " )"
                    "( "(->nanopass sentencias) "))")]

    [(metodo nombre argumentos tipo sentencias)
     (string-append "(metodo " (->nanopass nombre) " "
                    "( "(->nanopass argumentos) " ) "
                    (->nanopass tipo) " "
                    "( "(->nanopass sentencias) "))")]

    [(while guardia sentencias)
     (string-append "(while "
                    (->nanopass guardia) " "
                    "( "(->nanopass sentencias) " ))")]

    [(if-struct guardia then-sentencias else-sentencias)
     (string-append "(if-s "
                    (->nanopass guardia) " "
                    "( "(->nanopass then-sentencias) " ) "
                    "( "(->nanopass else-sentencias) " ))")]

    [(asig id valor)
     (string-append "(= "
                    (->nanopass id) " "
                    (->nanopass valor) ")")]

    [(asig-m ids valor)
     (let ([valor-str (->nanopass valor)])
       (foldr (lambda (id acc) (string-append "(= " (->nanopass id) " " valor-str ") " acc)) "" ids))]

    [(asig-t id tipo valor)
     (string-append "(: "
                    (->nanopass id) " "
                    (->nanopass tipo) ") "
                    "(= "
                    (->nanopass id) " "
                    (->nanopass valor) ")")]

    [(decla id tipo)
     (string-append "(: " (->nanopass id) " "
                    (->nanopass tipo) ")")]

    [(decla-m tipo ids)
     (let ([tipo-str (->nanopass tipo)])
       (foldr (lambda (id acc) (string-append "(: " (->nanopass id) " " tipo-str ") " acc)) "" ids))]

    [(arr-estruc elementos)
     (string-append "(arr-estruc " (->nanopass elementos) " )")]

    [(entrada-arr id indice)
     (string-append "(get-elem " (->nanopass id) " "
                    (->nanopass indice) ")")]

    [(id i) i]

    [(llamada nombre argumentos)
     (string-append "(" (->nanopass nombre) "("
                    (->nanopass argumentos) "))")]

    [(length nombre)
     (string-append "(length " (->nanopass nombre) ")")]

    ;[(println nombre)
     ;(string-append "(println( " (->nanopass nombre) ")")]

    [(bin-expr '+= e1 e2)
     (string-append "(= " (->nanopass e1) " (+ "
                    (->nanopass e1) " "
                    (->nanopass e2)"))")]

    [(bin-expr '-= e1 e2)
     (string-append "(= " (->nanopass e1) " (- "
                    (->nanopass e1) " "
                    (->nanopass e2)"))")]

    [(bin-expr op e1 e2)
     (string-append "(" (symbol->string op) " "
                    (->nanopass e1) " "
                    (->nanopass e2) ")")]

    [(un-expr e '++)
     (string-append "(= " (->nanopass e) " (+ "
                    (->nanopass e) " 1))")]

    [(un-expr e '--)
     (string-append "(= " (->nanopass e) " (- "
                    (->nanopass e) " 1))")]

    [(un-expr op e)
     (string-append "(" (symbol->string op) " "
                    (->nanopass e) ")")]

    [(if-corto guardia then-sentencias else-sentencias)
     (string-append "(if-c "
                    (->nanopass guardia) " "
                    (->nanopass then-sentencias) " "
                    (->nanopass else-sentencias) " )")]


    [(return nombre)
     (string-append "(return " (->nanopass nombre) ")")]

    [(tipo t)
     (match t
       ['INT      (symbol->string 'INT)]
       ['BOOL     (symbol->string 'BOOL)]
       ;['STR      (symbol->string 'STR)]
       ['ARR-INT  (symbol->string 'ARR-INT)]
       ['ARR-BOOL (symbol->string 'ARR-BOOL)])]

    [(numero n) n]

    [(bool b) b]

    ;[(string s) s]

    [void ""]

    ))

(define-language jelly

  (terminals
   (constante    (c))
   (id           (i))
   (operador     (op))
   (tipo         (t)))

  (Programa (p)
            (programa m)
            (programa m (cp* ...)))

  (Main (m)
        (main (s* ...)))

  (Cuerpo (cp)
          met
          func)

  (Funcion (func)
           (funcion i (d* ...) (s* ...)))

  (Metodo (met)
          (metodo i (d* ...) t (s* ... s1)))

  (Sentencia (s)
             (if-s e0 (s1* ...) (s2* ...))
             (while e0 (s1* ...))
             (= e s)                       ; ASIGNACION            
             (return e)
             d
             e)
             
  (Decla (d)
         (: i t))

  (Expr (e)
        (length i)               ; LENGTH
        ;(println i)              ; PRINT vars
        ;(println c)              ; PRINT cons
        (i (e* ...))             ; LLAMADA
        (arr-estruc e* ...)      ; ESTRUCTURA DEL ARREGLO 
        (if-c e0 e1 e2)          ; IF CORTO / OPERADOR TERNARIO
        (get-elem i e1)          ; ACCEDER A POSICION DEL ARREGLO
        (op e0 e1)
        (op e0)
        c
        i))

(define (constante? c)
  (or (number? c) (my-boolean? c)
      ;(my-string? c)
      ))
(define (id? i)
  (symbol? i))
(define (tipo? t)
  (memq t '(INT BOOL
                ;STR
                ARR-INT ARR-BOOL)))
(define (operador? op)
  (memq op '(= + - / % * > < & <= => == != += -= ! or and :)))
(define (my-boolean? c)
        (memq c '(True False)))
;(define (my-string? s)
        ;(eq? 0 (length (regexp-match #rx"'[*A-Za-z0-9_ ]+'" s))))

(define-parser parser-jelly jelly)

;Convierte la salida de ->nanopass a un simbolo como se pide en define-language jelly
(define (convertir-a-entrada salida)
  (read (open-input-string salida)))

; Desde la practica 1 hasta la practica de define-language
(define (aceptado? archivo)
  (let* ([in (open-input-file archivo)]
         [asc (jelly-parser (lex jelly-lexer in))]
         [asa (->nanopass asc)]
         [ase (convertir-a-entrada asa)]
         [asi (parser-jelly ase)])
    (begin
      (close-input-port in)
      asi)))

(define (get-rep-nano archivo)
  (let* ([in (open-input-file archivo)]
         [asc (jelly-parser (lex jelly-lexer in))]
         [asa (->nanopass asc)])
    (begin
      (close-input-port in)
      asa)))

;(jelly-parser (lex jelly-lexer (open-input-string "main(){x = 'prueba'}")))
;(->nanopass (jelly-parser (lex jelly-lexer (open-input-string "main(){x = 2}"))))
;(->nanopass (jelly-parser (lex jelly-lexer (open-input-string "main(){x:str}"))))
;(->nanopass (jelly-parser (lex jelly-lexer (open-input-string "main(){x = 'prueba'}"))))
;(->nanopass (jelly-parser (lex jelly-lexer (open-input-string "main(){x = 'prueba' println(x)}"))))
;(->nanopass (jelly-parser (lex jelly-lexer (open-input-string "main(){println('prueba')}"))))