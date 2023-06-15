#lang nanopass
(provide (all-defined-out))
(require "renombrado.rkt"
         "nanopass-jelly.rkt")

(define (tipos-programa ir tb)
  (nanopass-case (jelly Programa) ir
                 [(programa ,[tipos-main : m tb -> *]) tb]

                 [(programa  ,[tipos-main : m tb -> *] (,[tipos-cuerpo : cp* tb -> cp1] ...)) tb]))

(define (tipos-main ir tb)
  (nanopass-case (jelly Main) ir
                 [(main (,[tipos-sent : s* tb -> *] ...)) tb]))

(define (tipos-cuerpo ir tb)
  (nanopass-case (jelly Cuerpo) ir
                 [,met (begin
                         (tipos-met met tb)
                         tb)]

                 [,func (begin
                          (tipos-func func tb)
                          tb)]))

(define (tipos-func ir tb)
  (nanopass-case (jelly Funcion) ir
                 [(funcion ,i (,[tipos-decla : d* tb -> d1] ...) (,[tipos-sent : s* tb -> s1] ...))

                  (begin
                    (define args (map tipo-args d*))
                    (hash-set! tb i args)
                    tb)]))

(define (tipos-met ir tb)
  (nanopass-case (jelly Metodo) ir
                 [(metodo ,i (,[tipos-decla : d* tb -> d1] ...) ,t (,[tipos-sent : s* tb -> *] ... ,[tipos-sent : s1 tb -> s2]))
                  (begin
                    (define args (cons (map tipo-args d*) t ))
                    (hash-set! tb i args)
                    tb)]))

(define (tipo-args ir)
  (nanopass-case (jelly Decla) ir
                 [(: ,i ,t) t]))

(define (tipos-sent ir tb)
  (nanopass-case (jelly Sentencia) ir
                 [(if-s ,e0 (,[s1*] ...) (,[s2*] ...)) tb]

                 [(while ,e0 (,[s1*] ...)) tb]

                 ; [(length ,i) tb]

                 [(= ,e ,[s]) tb]

                 ; [(,i (,e* ...)) tb]

                 ; [(arr-estruc ,e* ...) tb]

                 [(return ,e) tb]

                 [,d (begin
                       (tipos-decla d tb)
                       tb)]

                 [,e tb]))


(define (tipos-decla ir tb)
  (nanopass-case (jelly Decla) ir
                 [(: ,i ,t) (hash-set! tb i t)]))

(define (get-tabla archivo)
  (let* ([renombrado (renombrar archivo)]
         [salida (tipos-programa renombrado (make-hash))])
    salida))