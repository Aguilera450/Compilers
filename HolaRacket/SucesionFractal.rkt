#lang racket

(provide fractal)

;; Suseciones Fractal.
(define (fractal n-esimo)
  (fractal/cc n-esimo '(0) 1 0))

;; Función que obtiene la sucesión fractal con recursión de cola.
(define (fractal/cc n-esimo lista pos acc)
  (if(>= (length lista) n-esimo)
     lista
     (if(equal? (modulo pos 2) 0)
        (fractal/cc n-esimo (append lista (list (+ acc 1))) (+ pos 1) (+ acc 1))
        (fractal/cc n-esimo (append lista (list (list-ref lista acc))) (+ pos 1) acc))))
