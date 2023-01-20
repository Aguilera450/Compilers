#lang racket
(provide anagramas-de)

;; Anagramas.
(define (anagramas-de str lista)
  (anagramas-de/k str lista (lambda(x)x)))

;; Verificamos que dos cadenas sean iguales.
(define (igual str1 str2)
  (if(equal? (sort (string->list str1) char<?) (sort (string->list str2) char<?))
     #t
     #f))

#| Comparamos las cadenas de la lista con la cadena a contrastar para devolver el
 número de anagramas en la lista, esto se realiza con ayuda de Continuation Passing Style. |#
(define (anagramas-de/k str lista k)
  (if(= (length lista) 0)
     (k '())
     (if(igual str (first lista))
        (anagramas-de/k str (list-tail lista 1) (lambda(v) (k(append (list (first lista)) v))))
        (anagramas-de/k str (list-tail lista 1) k))))
