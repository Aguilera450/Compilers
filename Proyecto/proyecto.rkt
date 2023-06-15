#! /usr/bin/env racket

#lang nanopass
(provide (all-defined-out))
(require "nanopass-jelly.rkt"
         "verificacion-tipos.rkt"
         "tabla-simbolos.rkt"
         "renombrado.rkt")
(require racket/cmdline)

(define (java-programa ir nombre)
  (nanopass-case (jelly Programa) ir
                 [(programa ,m) 
                  (string-append "public class" nombre " { \n " (java-main m) " \n }")]

                 [(programa ,[java-main : m -> m1] (,[java-cuerpo : cp* -> cp] ...)) 
                  (string-append "public class " nombre " { \n " m1 " \n " (to-string cp " \n ") " \n }")]))

(define (java-main ir)
  (nanopass-case (jelly Main) ir
                 [(main (,[java-sent : s* -> s] ...)) 
                  (string-append "public static void main (String[] args) { \n " (to-string s "; \n ") "; \n }")]))

(define (java-cuerpo ir)
  (nanopass-case (jelly Cuerpo) ir
                 [,met (java-met met)]

                 [,func (java-func func)]))

(define (java-func ir)
  (nanopass-case (jelly Funcion) ir
                 [(funcion ,i (,[java-decla : d* -> st1] ...) (,[java-sent : s* -> st2] ...)) 
                  (string-append "static void " (symbol->string i) " (" (to-string st1 ", ") ") { \n " (to-string st2 "; \n ") "; \n }")]))

(define (java-met ir)
  (nanopass-case (jelly Metodo) ir
                 [(metodo ,i (,[java-decla : d* -> st1] ...) ,t (,[java-sent : s* -> st2] ... ,[java-sent : s1 -> st3])) 
                  
                  (if (string-contains? st3 "return")
                    (string-append "static " (tipo-en-java t) " " (symbol->string i) " (" (to-string st1 ", ") ") { \n " (to-string st2 "; \n ") st3 "; \n }")
                    (string-append "static " (tipo-en-java t) " " (symbol->string i) " (" (to-string st1 ", ") ") { \n " (to-string st2 "; \n ") " return " st3 "; \n }"))]))

(define (java-sent ir)
  (nanopass-case (jelly Sentencia) ir
                 [(if-s ,[java-expr : e0 -> se0] (,[s1*] ...) (,[s2*] ...)) 
                  (string-append "if (" se0 ") { \n " (to-string s1* "; \n ") "; \n } \n else { \n " (to-string s2* "; \n ") "; \n }")]

                 [(while ,[java-expr : e0 -> se0] (,[s1*] ...)) 
                  (string-append "while (" se0 ") { \n " (to-string s1* "; \n ") "; \n }")]

                 [(= ,[java-expr : e -> se0] ,[s]) 
                  (let* ([t (obtener-tipo-arreglo se0)])
                       (if(boolean? t)
                                    (string-append se0 " = " s)
                                    (string-append se0 " = new " t s)))
                  ]

                 [(return ,[java-expr : e -> se0]) 
                  (string-append "return " se0)]

                 [,d (java-decla d)]

                 [,e (java-expr e)]))

(define (java-decla ir)
  (nanopass-case (jelly Decla) ir
                 [(: ,i ,t) (let ([t1 (tipo-en-java t)])
                                 (if(or (string=? t1 "int[]")(string=? t1 "boolean[]"))
                                    (agregar-arreglo (symbol->string i) t1)
                                    (void))
                                 (string-append t1 " " (symbol->string i))
                             )]))

(define (java-expr ir)
  (nanopass-case (jelly Expr) ir

                 [(length ,[java-expr : i -> si]) 
                  (string-append si ".length")]

                 ;[(println ,[java-expr : i -> si]) 
                  ;(string-append "println(" si ")")]

                 ;[(println , c) 
                  ;(string-append "println(" c ")")]
                 
                 [(,[java-expr : i -> si] (,[e*] ...))
                  (string-append si " (" (to-string e* ", ") ")")]

                 [(arr-estruc ,[e*] ...) 
                  (string-append "{" (to-string e* ", ") "}")]

                 [(if-c ,[e0] ,[e1] ,[e2]) 
                  (string-append e0 " ? " e1 " : " e2 )]

                 [(get-elem ,[java-expr : i -> si] ,[e1]) 
                  (string-append si "[" e1 "]")]

                 [(,op ,[e0] ,[e1]) 
                  (string-append e0 " " (symbol->string op) " " e1)]

                 [(,op ,[e0]) 
                  (string-append (symbol->string op) " " e0)]

                 [,c (if (number? c) (number->string c) (bool-en-java c))]

                 [,i (symbol->string i)]
                 
                 [else "CASO ELSE"]))

(define (to-string lista sep)
(if (empty? lista) 
  ""
(let* 
  ([tamanio (- (length lista) 1)]
   [str ""])
  
(for ([i (in-range tamanio)])
    (set! str (string-append str (list-ref lista i) sep)))
    (string-append str (last lista)))
   ))


(define (bool-en-java b)
(match b
['True "true"]
['False "false"]))

(define (tipo-en-java tipo)
(match tipo
['INT "int"]
['BOOL "boolean"]
;['STR "String"]
['ARR-INT "int[]"]
['ARR-BOOL "boolean[]"]))

;--------- Con esta tabla hash controlamos los arreglos ------------
(define hash-arreglos
  (make-hash))

(define (agregar-arreglo nombre tipo)
  (hash-set! hash-arreglos nombre tipo))

(define (obtener-tipo-arreglo nombre)
  (hash-ref hash-arreglos nombre #f))
;-------------------------------------------------------------------
(define nombre-clase (vector-ref (current-command-line-arguments) 1))

(define (get-rep-java archivo nombre)
    (let* ([entrada (aceptado? archivo)]
           [e-renombrada  (rename-var entrada)]
           [tabla (tipos-programa e-renombrada (make-hash))])
        (type-check e-renombrada tabla)   
        (java-programa entrada nombre)))

(display (get-rep-java (vector-ref (current-command-line-arguments) 0) nombre-clase))
