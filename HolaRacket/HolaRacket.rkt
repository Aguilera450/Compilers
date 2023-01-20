#lang plai

; Importando las funciones requeridas de los módulos en donde se declaran.
(require "Hamming.rkt")
(require "Anagramas-de.rkt")
(require "SucesionFractal.rkt")
;(require "ArbolesDivisores.rkt")

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST Hamming:
(define (test-string-hamming)
  (test (hamming "ducky" "lucky") 1))

(define (test-number-hamming)
  (test (hamming 1011101 1001001) 2))

(define (test-list-hamming)
  (test (hamming (list #t #f) (list #t #t)) 1))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST Anagramas-de:
(define (test-anagramas)
  (test (anagramas-de "abatirse" '("abiertas" "rabietas" "baterias" "compiladores"))
        '("abiertas" "rabietas" "baterias")))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST SucesionFractal:
(define (test-fractal)
  (test (fractal 20) '(0 0 1 0 2 1 3 0 4 2 5 1 6 3 7 0 8 4 9 2)))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST ArbolesDivisores:
;(define (test-div-tree)
;  (test (div-tree 20) (node 20 (leaf 2) (node 10 (leaf 2) (leaf 5)))))

;(define (test-prime-fac)
;  (test (prime-fac 90) '(2 3 3 5)))
