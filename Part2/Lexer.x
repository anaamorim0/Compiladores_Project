-- Scanner for the lexical analysis phase
{
module Lexer where
}

%wrapper "basic"

$white = [\ \t\n\r]             -- espaços em branco
$digit = [0-9]                  -- digitos de 0 a 9
$alpha = [a-zA-Z]               -- letras (maiusculas e minusculas)
$alpha_num = [$alpha $digit _]  -- combinação de numeros e letras
$id_start = [$alpha _]          -- o identificador pode começar com uma letra ou um _

tokens :-

$white+    ;   -- Ignorar carateres "brancos"


"//".*     ;   -- Ignorar comentários
"/*"(.|\n)*"*/"      ;      -- Ignorar comentários de várias linhas


-- Expressões aritmeticas
"+"         { \_ -> PLUS }
"-"         { \_ -> MINUS }
"*"         { \_ -> MULT }
"/"         { \_ -> DIV }
"%"         { \_ -> MOD }

-- Expressões booleanas
"true"      { \_ -> BOOL True }
"false"     { \_ -> BOOL False}

"=="        { \_ -> EQUAL}
"!="        { \_ -> NOTEQUAL}
"<"         { \_ -> LESST }
">"         { \_ -> GREATERT }
">="        { \_ -> GREATEQUAL}
"<="        { \_ -> LESSEQUAL}

-- Operadores lógicos
"&&"        { \_ -> AND }
"||"        { \_ -> OR }
"!"         { \_ -> NOT }

-- Palavras-chave
"var"       { \_ -> VAR }
"val"       { \_ -> VAL }
"println"   { \_ -> PRINTLN }
"print"     { \_ -> PRINT }
"readln"    { \_ -> READLN }
"return"    { \_ -> RETURN }
"fun"       { \_ -> FUN }
"main"      { \_ -> MAIN }

-- Declarações e atribuições de variáveis
"="         { \_ -> ASSIGN }

-- Expressões condicionais
"if"        { \_ -> IF }
"else"      { \_ -> ELSE }


-- While loops
"while"     { \_ -> WHILE}

"("         { \_ -> LPAREN }
")"         { \_ -> RPAREN }
"{"         { \_ -> LBRACE}
"}"         { \_ -> RBRACE}

--Números
"-"?$digit+("." $digit+)?       { \s -> NUM (read s :: Float) }

--Identificadores
$id_start[$alpha_num _]*       { \s -> ID s}

--Strings
\"[^\"]*\"                  { \s -> STRING (init (tail s)) } --remove as aspas 

{
data Token
    = PLUS
    | MINUS
    | MULT
    | DIV
    | MOD
    | BOOL Bool
    | EQUAL
    | NOTEQUAL
    | LESST
    | GREATERT
    | GREATEQUAL
    | LESSEQUAL
    | AND
    | OR
    | NOT
    | VAR
    | VAL
    | PRINTLN
    | PRINT
    | READLN
    | RETURN
    | FUN
    | MAIN
    | ASSIGN
    | IF
    | ELSE
    | WHILE
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | NUM Float
    | ID String
    | STRING String 
    deriving (Eq, Show)
}
