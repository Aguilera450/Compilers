#lang nanopass
(provide (all-defined-out))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


;; Lexer y Tokens.

; Token para contenedores, identificadores
(define-tokens contenedores (ID NUM BOOLEAN STRING))


; Las palabras reservadas son tokens vacios. 
; Tokens de tipos, e.g. bool => INT
(define-empty-tokens vacios
  (INT BOOL
   IF ELSE RETURN WHILE FOR               ; Palabras reservadas
   LP RP LC RC LCB RCB DOTS QSTN COMMA    ; ( ) [ ] { } : ? ,
   ADD SUBS DIV MULT MOD                  ; + - / * %
   EQ NEQ LT GT LTEQ GTEQ                 ; == != < > <= >=
   ASG ADDASG SUBSASG                     ; = += -=
   AUTOINC AUTODEC                        ; ++ --
   AND OR NOT                             ; & | !
   EOF MAIN
   LENGTH PRINT                                 ; Funciones
   ))                              

(define jelly-lexer
  (lexer
   ; [(Exp. regular) (token que se debe generar)]
   [(:: "{-" (complement (:: any-string "-}" any-string)) "-}") (jelly-lexer input-port)] ; MULTILINEA
   [(:: "//" (complement (:: any-string "\n" any-string)) "\n") (jelly-lexer input-port)] ; UNA LINEA
   ;[(:: "'" (complement (:: any-string "'" any-string)) "'") (token-STRING lexeme)] ; CADENAS
   [whitespace       (jelly-lexer input-port)] ; Espacios vacíos.
   ["<="      (token-LTEQ)]
   [">="      (token-GTEQ)]
   ["=="      (token-EQ)]
   ["!="      (token-NEQ)]
   ["+="      (token-ADDASG)]
   ["-="      (token-SUBSASG)]
   ["++"      (token-AUTOINC)]
   ["--"      (token-AUTODEC)]
   ["bool"    (token-BOOL)]
   ["int"     (token-INT)]
   ;["str"     (token-STR)]             ;Str type token
   ["while"   (token-WHILE)]
   ["for"     (token-FOR)]
   ["if"      (token-IF)]
   ["else"    (token-ELSE)]
   ["return"  (token-RETURN)]
   ["+"       (token-ADD)]
   ["-"       (token-SUBS)]
   ["/"       (token-DIV)]
   ["*"       (token-MULT)]
   ["%"       (token-MOD)]
   ["<"       (token-LT)]
   [">"       (token-GT)]
   ["&"       (token-AND)]
   ["|"       (token-OR)]
   ["!"       (token-NOT)]
   ["("       (token-LP)]
   [")"       (token-RP)]
   ["["       (token-LC)]
   ["]"       (token-RC)]
   ["{"       (token-LCB)]
   ["}"       (token-RCB)]
   ["="       (token-ASG)]
   [":"       (token-DOTS)]
   ["?"       (token-QSTN)]
   [","       (token-COMMA)]
   ["length"  (token-LENGTH)]
   ;["println" (token-PRINT)]
   ["main"    (token-MAIN)]
   [(:or "True" "False") (token-BOOLEAN lexeme)]
   [(:+ (char-range #\0 #\9)) (token-NUM lexeme)]
   [(:: (char-range #\a #\z) (:* (:or alphabetic numeric #\_))) (token-ID lexeme)]
   [(eof) (token-EOF)]
   ))

; (define (lexea s)
;   (let* ([input (open-input-file s)]
;          [token (jelly-lex input)]
;          [tokens (list token)])
;     (until (eq? token 'EOF)
;            (begin
;              (set! token (jelly-lex input))
;              (set! tokens (cons token tokens))))
;     (reverse tokens)))

; Ejemplo: (jelly-lexer (open-input-string "+ 2"))
; (jelly-lexer (open-input-string "//hola \n + 2"))
;(jelly-lexer (open-input-string "'asdfaf'"))