#lang nanopass
(require "parser-ej.rkt"
         "lexer-ej.rkt")

;Definicion del lenguaje:
(define-language ejemplo
    (terminals
      (constante (c))
      (primitivo (pr))
      (tipo (t))
      (id (i)))
    (Programa (p)
        meth
        (programa m meth))
    (Main (m)
        (main [e* ... e]))
    (Metodo (meth)
        (metodo i   ([i* t*] ...) t e))
    (Expr (e)
        c
        i
        pr
        (if-stn e0 e1 e2)
        (return e)
        (pr e0 e1)
        (e* ... e)))


;Predicados necesarios para reconocer los terminales.
(define (id? c) (symbol? c))
(define (tipo? p) (memq p '(int bool)))
(define (primitivo? p) (memq p '(+ > =)))
(define (constante? c) (number? c))

;Definición del parser del lenguaje ejemplo.
(define-parser parser-ejemplo ejemplo)

;Primer ejemplo del nanopass-case
(define (ejemplo1 ir)
    (nanopass-case (ejemplo Metodo) ir
        [(metodo ,i   ([,i* ,t*] ...) ,t  ,e)   t*]))


#|Segundo ejemplo de un nanopass-case
  Nota como se pueden utilizar diferentes nanopass-case segun la variable|#
(define (vars-met ir)
   (nanopass-case (ejemplo Metodo) ir
        [(metodo ,i   ([,i* ,t*] ...) ,t  ,e) (let ([vars (mutable-set)])
                                                        (set-union! vars (list->mutable-set i*))
                                                        (set-union! vars (vars-exp e))
                                                        vars)]))
#|Tercer ejemplo de un nanopass-case
  Nota como se pueden utilizar diferentes nanopass-case segun la variable|#
(define (vars-exp ir)
   (nanopass-case (ejemplo Expr) ir
        [,i   (mutable-set i)]

        [(if-stn ,e0 ,e1 ,e2)  (let ([variables (mutable-set)])
                                    (set-union! variables (vars-exp e0))
                                    (set-union! variables (vars-exp e1))
                                    (set-union! variables (vars-exp e2))
                                    variables)]
        [(,pr ,e0 ,e1) (let ([variables (mutable-set)])
                                    (set-union! variables (vars-exp e0))
                                    (set-union! variables (vars-exp e1))
                                    variables)]
        [(return ,e) (vars-exp e)]
        [(,e* ... ,e) (let ([set-vars (mutable-set)])
                            (for-each (lambda (v) (set-union! set-vars v)) (map vars-exp e*))
                            (set-union! set-vars (vars-exp e))
                            set-vars)]
        [else (begin (display ir)
                     (print "caso else")
                     (mutable-set))])) ;<---- Dejar displays y prints sirve mucho
                                             ;para saber cuando esta cayendo en este caso
                                             ;o lo que esta llegando a algun caso.



;Función que toma el código de un fichero y le aplica cada practica hasta ->nanopass
(define (parsea2 s)
  (let* ([a (open-input-file s)]
         [r (jelly-parser (lex jelly-lexer a))]
         [r2 (->nanopass r)])
    (begin  (close-input-port a)
             r2)))

; entrada es el resultado de aplicar parsea2 a in.jly
(define entrada '(programa
                (main ((if-stn (> a b) (= j(+ 1 j)) (= i(+ 1 i))) (= i(+ 1 i)) (= j(+ 1 j))))
                (metodo gdc [(varu int)(vard int)] int ((if-stn (> varu vard) (+ vard varu) (+ varu vard))(return b)))))

; entrada-p es el resultado de parsear con el lenguaje de nanopass a entrada
(define entrada-p (parser-ejemplo entrada))


;met es solo el metodo de entrada
(define met '(metodo gdc [(varu int)(vard int)] int ((if-stn (> varus vardz) (+ vard varu) (+ varu vard)) (return b) ) ) )

#|Para probar nuestra definicion hecha con nanopass-case primero parseamos con parser-ejemplo
  y despues llamamos a la funcion, en este caso se llama vars-met|#
(define variables-de-met (vars-met (parser-ejemplo met)))

;nanopass-case <-- A partir de una representacion intermedia, construir algo
;define-pass   <-- A partir de una representacion intermedia, construir otra representacion intermedia
