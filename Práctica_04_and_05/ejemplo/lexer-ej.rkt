#lang nanopass
(provide contenedores
          vacios
          jelly-lexer)

(require parser-tools/lex
         br/cond
         (prefix-in : parser-tools/lex-sre))

(define-tokens contenedores (NUM ID))
(define-empty-tokens vacios (LP MAIN > ++ ELSE LK RK IF
                             INT
                             BOOLEAN
                             RETURN
                             RP
                             NOT
                             +
                             COMA
                             WHILE
                             :
                             +=
                             EOF))


(define jelly-lexer
  (lexer
        [(:: "{-" (:* (complement "-}")) "-}") (jelly-lexer input-port)]
        [(:: "main" )  (token-MAIN)]
        [(:: "++" )  (token-++)]
        [(:: "return" )  (token-RETURN)]
        [(:: "if" )  (token-IF)]
        [(:: "int" )  (token-INT)]
        [(:: "boolean" )  (token-BOOLEAN)]
        [(:: "else" )  (token-ELSE)]
        [(:: "while" )  (token-WHILE)]
        [(:: "," )  (token-COMA)]
        [(:: "{" )  (token-LK)]
        [(:: "}" )  (token-RK)]
        [(:: ">" )  (token->)]
        [(:: "(" )  (token-LP)]
        [(:: ")" )  (token-RP)]
        [(:: ":" )  (token-:)]
        [(:: "+=" )  (token-+=)]
        [(:: #\+ )  (token-+)]
        [(:: #\! )  (token-NOT)]
        [(:+ (char-range #\0 #\9)) (token-NUM (string->number lexeme))]
        [(:+ (char-range #\a #\z)) (token-ID lexeme)]
        [whitespace (jelly-lexer input-port)]
        [(eof)      (token-EOF)]))


