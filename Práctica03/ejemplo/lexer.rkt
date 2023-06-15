#lang nanopass
(provide contenedores
          vacios
          jelly-lexer)

(require parser-tools/lex
         br/cond
         (prefix-in : parser-tools/lex-sre))

(define-tokens contenedores (NUM ID))
(define-empty-tokens vacios (LP
                             RP
                             NOT
                             ADD
                             :
                             +=
                             EOF))


(define jelly-lexer
  (lexer
        [(:: "(" )  (token-LP)]
        [(:: ")" )  (token-RP)]
        [(:: ":" )  (token-+=)]
        [(:: "+=" )  (token-:)]
        [(:: #\+ )  (token-ADD)]
        [(:: #\! )  (token-NOT)]
        [(:+ (char-range #\0 #\9)) (token-NUM (string->number lexeme))]
        [(:+ (char-range #\a #\z)) (token-ID lexeme)]
        [whitespace (jelly-lexer input-port)]
        [(eof)      (token-EOF)]))


