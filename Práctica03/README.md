# Compiladores 2023-2
Práctica 03.
## Integrantes:
- Adrian Aguilera Moreno.
  - No. cuenta. `421005200`.
  - Cuenta github. `@Aguilera450`.
  - Correo. aguilera@ciencias.unam.mx
  
- Aquino Chapa Armando Abraham
  - No. cuenta. `317058163`.
  - Cuenta github. `@ArmandoAAC11`
  - Correo. armandoaac@ciencias.unam.mx
  
- Rojas Reyes Saul Adrian
  - No. cuenta. `114006224`.
  - Cuenta github. `@RRSaul`
  - Correo. wannacry@ciencias.unam.mx

- Gutiérrez Medina Sebastián Alejandro
  - No. cuenta. `318287021`.
  - Cuenta github. `@SGM2099`
  - Correo. sebasguti1511@ciencias.unam.mx

## Observaciones.
- ...
## ¿Cómo correr la práctica?

Situarse en la *terminal* a la misma altura que el archivo `my_parser.rkt` y ejecutar los siguientes comandos:

```bash
racket
```
```bash
(enter! "my_parser.rkt")
```
Para ejecutar el **parser**, basta situarse en el archivo `my_parser.rkt` y en la línea final, dentro de las comillas insertar el programa:
```bash
(jelly-parser (lex jelly-lex (open-input-string "--{ 1 == 4 + True }")))
```

## Gramatica de Jelly

```bash

prog ->  proc ,  . . .
procint ->  id (decl, . . .):  int {  expre \n return expreint }  \n 
procbool ->  id (decl, . . .):  bool {  expre \n return exprebool }  \n 
procintarray -> id (decl, . . .):  int {  expre \n return expreint }  \n 
procboolarray -> id (decl, . . .):  bool {  expre \n return exprebool }  \n 

proc -> id (decl, . . .) {  expre }  \n 
decl -> id : type 

expre -> expre \n expre
    | comment expre
    | expre comment
    |  id : int = expreint
    |  id : bool = exprebool
    | id: int[] = { expreint, . . .}
    | id: bool[] = {exprebool, . . .}
    | type id, . . . 
    | id = expre
    | id = expreint
    | id = exprebool
    | id += id
    | id += expreint
    | id -= id   
    | id -= expreint
    | id ++
    | id - - 
    | proc ( id, . . . ) 
    | while (exprebool) { expre }
    | if (exprebool) prog else prog
    | if (exprebool) prog
    | exprebool ? expre : expre
    | return id
    | return exprebool
    | return expreint

expreboolarray -> {exprebool, . . .}
    | id (id,  . . .)
expreintarray -> {exprebool, . . .}
    | id (id,  . . .)

exprebool -> exprebool <= exprebool 
    | expre == expre
    | expre != expre
    | expreint < expreint
    | expreint > exprebool
    | expreint <= expreint
    | expreint >= expreint
    | exprebool & exprebool
    | exprebool | exprebool
    | !exprebool
    | id (id,  . . .)
    | id [expreint]
    | false, true

expreint -> expreint + expreint
    | expreint - expreint
    | expreint * expreint
    | expreint / expreint
    | id (id,  . . .)
    | expreint % expreint
    | id [expreint]
    | n
    | n+

n -> . . ., -2 , -1 , 0, 1, 2, . . .
n+  -> 0, 1, 2, . . .

type -> bool | int 
id -> [a-zA-Z_$][a-zA-Z_$0-9]*

lcomment -> --.*\n*
mlcomment -> {- -* ([^ * /] | [^ - ]} | -[^ } ])* }*  -}
```