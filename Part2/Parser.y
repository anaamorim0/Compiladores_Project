-- Parser that returns the abstract syntax tree (AST) representing the Kotlin program given as input
{
module Parser where
import Lexer  
}

%name parse
%tokentype { Token }
%error { parseError }

%token
     PLUS           { PLUS }
     MINUS          { MINUS }
     MULT           { MULT }
     DIV            { DIV }
     MOD            { MOD }
     BOOL           { BOOL b }
     EQUAL          { EQUAL }
     NOTEQUAL       { NOTEQUAL }
     LESST          { LESST }
     GREATERT       { GREATERT }
     GREATEQUAL     { GREATEQUAL }
     LESSEQUAL      { LESSEQUAL }
     AND            { AND }
     OR             { OR }
     NOT            { NOT }
     VAR            { VAR }
     VAL            { VAL }
     PRINTLN        { PRINTLN }
     PRINT          { PRINT }
     READLN         { READLN }
     RETURN         { RETURN }
     FUN            { FUN }
     MAIN           { MAIN }
     ASSIGN         { ASSIGN }
     WHILE          { WHILE }
     IF             { IF }
     ELSE           { ELSE }
     LPAREN         { LPAREN }
     RPAREN         { RPAREN }
     LBRACE         { LBRACE }
     RBRACE         { RBRACE }
     NUM            { NUM n }
     ID             { ID s }
     STRING         { STRING str }


-- Definições de precedência
%left OR
%left AND
%left EQUAL NOTEQUAL GREATEQUAL LESSEQUAL
%left LESST GREATERT
%left PLUS MINUS
%left MULT DIV MOD
%right NOT

%%

Program : MainFunction { Program $1 }

-- Definição da função principal(main)
MainFunction : FUN MAIN LPAREN RPAREN LBRACE List RBRACE    { Main $6 }
             | FUN MAIN LPAREN RPAREN LBRACE RBRACE         { Main [] }

-- Definição de uma lista de instruções
List : Inst List { $1 : $2 }
     | Inst      { [$1] }

-- Definição de expressões que podem ser operações aritméticas, comparações lógicas,...
Exp : Exp PLUS Exp              { Add $1 $3 }
    | Exp MINUS Exp             { Sub $1 $3 }
    | Exp MULT Exp              { Mul $1 $3 }
    | Exp DIV Exp               { Div $1 $3 }
    | Exp MOD Exp               { Mod $1 $3 }
    | Exp LESST Exp             { LessThan $1 $3 }
    | Exp GREATERT Exp          { GreaterThan $1 $3 }
    | Exp EQUAL Exp             { Equal $1 $3 }
    | Exp NOTEQUAL Exp          { NotEqual $1 $3 }
    | Exp GREATEQUAL Exp        { GreaterEqual $1 $3 }
    | Exp LESSEQUAL Exp         { LessEqual $1 $3 }
    | Exp AND Exp               { And $1 $3 }
    | Exp OR Exp                { Or $1 $3 }
    | NOT Exp                   { Not $2 }
    | BOOL                      { case $1 of BOOL b -> Bool b }
    | LPAREN Exp RPAREN         { $2 }
    | Term                      { $1 }


-- Definições de Instruções como declarações, atribuições, expressões if-then-else, while loops, prints...
Inst : WHILE Exp LBRACE List RBRACE                                        { While $2 $4 }
     | IF LPAREN Exp RPAREN LBRACE List RBRACE ELSE LBRACE List RBRACE     { IfElse $3 $6 $10 }
     | IF LPAREN Exp RPAREN LBRACE List RBRACE                             { If $3 $6 }
     | VAR ID ASSIGN Exp                                                   { case $2 of ID s -> Assign s $4 }
     | VAL ID ASSIGN Exp                                                   { case $2 of ID s -> Assign s $4 }
     | PRINTLN Exp                                                         { PrintLn $2 }
     | PRINT Exp                                                           { Print $2 }
     | RETURN Exp                                                          { Return $2 }
     | ID ASSIGN Exp                                                       { case $1 of ID s -> Assign s $3 }

-- Definição de termos, que podem ser números, identificadores, strings, ou expressões entre parentesis
Term : NUM                      { case $1 of NUM n -> Num n }
     | ID                       { case $1 of ID s -> Var s }
     | STRING                   { case $1 of STRING str -> Str str }
     | READLN LPAREN RPAREN     { ReadLn }

{
data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Bool Bool
         | Equal Exp Exp
         | NotEqual Exp Exp
         | LessThan Exp Exp
         | GreaterThan Exp Exp
         | GreaterEqual Exp Exp
         | LessEqual Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Assign String Exp
         | DeclareType String String
         | PrintLn Exp
         | Print Exp
         | ReadLn
         | Return Exp
         | Main [Exp]
         | While Exp [Exp]
         | If Exp [Exp]
         | IfElse Exp [Exp] [Exp]             
         | Num Float
         | Var String
         | Str String
         deriving (Show, Eq)

data Program = Program Exp
            deriving (Show, Eq)

parseError :: [Token] -> a
parseError toks = error "parse error"  
}