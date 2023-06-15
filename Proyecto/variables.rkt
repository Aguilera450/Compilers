#lang nanopass
(provide (all-defined-out))
(require "nanopass-jelly.rkt")

(define (vars-programa ir st)
  (nanopass-case (jelly Programa) ir
                 [(programa ,m) (begin
                                  (vars-main m st)
                                  st)]

                 [(programa ,[vars-main : m st -> *] (,[vars-cuerpo : cp* st -> st1] ...)) st]))

(define (vars-main ir st)
  (nanopass-case (jelly Main) ir
                 [(main (,[vars-sent : s* st -> *] ...)) st]))

(define (vars-cuerpo ir st)
  (nanopass-case (jelly Cuerpo) ir
                 [,met (begin
                         (vars-met met st)
                         st)]

                 [,func (begin
                          (vars-func func st)
                          st)]))

(define (vars-func ir st)
  (nanopass-case (jelly Funcion) ir
                 [(funcion ,i (,[vars-decla : d* st -> st1] ...) (,[vars-sent : s* st -> st2] ...)) st]))

(define (vars-met ir st)
  (nanopass-case (jelly Metodo) ir
                 [(metodo ,i (,[vars-decla : d* st -> st1] ...) ,t (,[vars-sent : s* st -> st2] ... ,[vars-sent : s1 st -> st3])) st]))

(define (vars-sent ir st)
  (nanopass-case (jelly Sentencia) ir
                 [(if-s ,[vars-expr : e0 st -> *] (,[s1*] ...) (,[s2*] ...)) st]

                 [(while ,[vars-expr : e0 st -> *] (,[s1*] ...)) st]

                 [(= ,[vars-expr : e st -> *] ,[s]) st]

                 [(return ,[vars-expr : e st -> *]) st]

                 [,d (begin
                       (vars-decla d st)
                       st)]

                 [,e (begin
                       (vars-expr e st)
                       st)]))

(define (vars-decla ir st)
  (nanopass-case (jelly Decla) ir
                 [(: ,i ,t) (set-add! st i)]))

(define (vars-expr ir st)
  (nanopass-case (jelly Expr) ir

                 [(length ,[vars-expr : i st -> *]) st]

                 ;[(println ,[vars-expr : i st -> *]) st]

                 ;[(println , c st]
                 
                 [(,i (,[e*] ...)) st]

                 [(arr-estruc ,[e*] ...) st]

                 [(if-c ,[e0] ,[e1] ,[e2]) st]

                 [(get-elem ,[vars-expr : i st -> *] ,[e1]) st]

                 [(,op ,[e0] ,[e1]) st]

                 [(,op ,[e0]) st]

                 [,c st]

                 [,i (set-add! st i)]))

(define (get-variables archivo)
  (let* ([entrada (aceptado? archivo)]
         [variables (vars-programa entrada (mutable-set))])
    variables))