#lang racket

(provide hamming)

;; Hamming: verifica si le pasamos {number, string, list}
(define (hamming str1 str2)
  (cond
    [(and (list? str1) (list? str2))
     (hamming-list/k str1 str2 (lambda(x)x))]
    [(and (number? str1) (number? str2))
     (hamming/k (number->string str1) (number->string str2) (lambda(x)x))]
    [else (hamming/k str1 str2 (lambda(x)x))]))

#| Si nuestras entradas son listas (se puede usar en el caso de tener una
cadena representada por una lista), entonces usamos continuation passing
style para realizar el conteo de las diferencias entre las listas. |#
(define (hamming-list/k list1 list2 k)
   (if(or (empty? list1) (empty? list2))
     (k 0)
     (if(equal? (first list1) (first list2))
        (hamming-list/k (cdr list1) (cdr list2) k)
        (hamming-list/k (cdr list1) (cdr list2) (lambda(v) (k (+ v 1)))))))

#| Si nuestras cadenas es de números o string, entonces convertimos todo a string
y utilizamos continuation passing style para realizar el conteo de las diferencias
por caracter. |#
(define (hamming/k str1 str2 k)
  (if(or (= (string-length str1) 0) (= (string-length str2) 0))
     (k 0)
     (if(char=? (string-ref str1 0) (string-ref str2 0))
        (hamming/k (substring str1 1) (substring str2 1) k)
        (hamming/k (substring str1 1) (substring str2 1) (lambda(v) (k(+ v 1)))))))
