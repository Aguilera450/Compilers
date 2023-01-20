#lang plai

;; Definición de estrura tree.
(define-type tree
  (leaf [v positive-integer?])
  (node [v positive-integer?] [h1 tree?] [h2 tree?]))

;; div-tree
(define (div-tree n)
    (div-tree-aux n (prime-fac n)))

;; Función auxiliar para construir el árbol de divisores.
(define (div-tree-aux n lista)
  (cond
    [(primo? n)
     (leaf n)]
    [(empty? lista)
     (leaf 1)]
    [else
    (node n (leaf (first lista)) (div-tree-aux (/ n (first lista)) (list-tail lista 1)))]))

;; Función que indica si un número es primo o no.
(define (primo? numero)
  (primoAux numero 2))

;; Funcion auxiliar de la función "primo?" que checa todos los requisitos para ver si es un numero primo.
(define (primoAux num div)
  (cond
    [(and (<= num 2) (= num 2)) #t]
    [(and (<= num 2) (not (= num 2))) #f]
    [(= (modulo num div) 0) #f]
    [(> (* div div) num) #t]
    [else (primoAux num (+ div 1))]))

;; prime-fac
(define (prime-fac n)
  (cond
    [(positive-integer? n)
     (prime-fac/cc n '() 2)]
    [(and (integer? n) (< n 2))
     "El número que usted a ingresado no tiene factores primos positivos."]     
    [else
     "Esta ingresando un valor no entero o algo que no es de tipo Number."]))

#| Función auxiliar que devuelve una lista con los factores primos de "n".
 Para esta función se utilizó recursión de cola. [TODO pasar a Continuation Passing Style.] |#
(define (prime-fac/cc n lista acc)
  (if(= n 1)
     lista
     (if(= (modulo n acc) 0)
        (prime-fac/cc (/ n acc) (append lista (list acc)) acc)
        (prime-fac/cc n lista (+ acc 1)))))

;; Test
(define (test-div-tree)
  (test (div-tree 20) (node 20 (leaf 2) (node 10 (leaf 2) (leaf 5)))))

(define (test-prime-fac)
  (test (prime-fac 90) '(2 3 3 5)))
