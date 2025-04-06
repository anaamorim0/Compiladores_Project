module ICG where            -- Intermediate Code Generator

import Parser (Exp(..), Program(..))        -- Importa os tipos do Parser
import SymbolTable                          -- Importa a tabela de Símbolos
import qualified Data.Map as Map


-- Definição do TAC (Three-Address Code)
data TAC
    = TACAssign String String                   -- Atribuições
    | TACBinOp String String String String      -- Operações Binárias
    | TACLogicalOp String String String String  -- Operações Lógicas
    | TACUnaryOp String String String           -- Operações unárias
    | TACCondJump String String String String   -- Saltos Condicionais
    | TACJump String                            -- Saltos Incondicionais 
    | TACLabel String                           -- Declaração de Lable
    | TACPrint String                           -- Print(..)
    | TACPrintLn String                         -- Println(..)
    | TACRead String                            -- Readln(..)
    | TACReturn String                          -- Return ..
    deriving (Show)


-- Definição do estado do gerador do TAC
data ICGState = ICGState
    { tempCounter :: Int                -- Contador de variáveis temporárias
    , labelCounter :: Int               -- Contador de labels
    , symbolTable :: ScopedSymbolTable  -- Tabela de Símbolos para controlar as variáveis e os âmbitos
    } deriving (Show)


-- Estado inicial
initialState :: ICGState
initialState = ICGState 0 0 emptySymbolTable        -- Contadores a zero e tabela de símbolos vazia


-- Função para gerar Variáveis Temporárias
newTemp :: ICGState -> (String, ICGState)
newTemp state =
    let temp = "t" ++ show (tempCounter state)
    in (temp, state { tempCounter = tempCounter state + 1 })        -- incrementa o contador


-- Função para gerar Labels
newLabel :: ICGState -> (String, ICGState)
newLabel state =
    let label = "L" ++ show (labelCounter state)
    in (label, state { labelCounter = labelCounter state + 1 })     -- incrementa o contador


-- Função para converter expressões numa TAC
generateExpr :: Exp -> Maybe String -> ICGState -> ([TAC], String, ICGState)
-- Numeros
generateExpr (Num n) _ state = ([], formatNum n, state)     -- Para numeros o resultado é o proprio valor
    where
        formatNum x
            | x == fromIntegral (round x) = show (round x :: Int) -- Assegurar que aparece como um Int
            | otherwise = show x
-- Variáveis
generateExpr (Var name) _ state = ([], name, state)         -- Para variáveis, o resultado é o nome da variável
-- Booleanos
generateExpr (Bool b) maybeDest state =
    let value = if b then "1" else "0"      -- Converte o valor booleano para uma string
        temp = case maybeDest of
            Just dest -> dest
            Nothing   -> value
    in ([], temp, state)
-- Soma
generateExpr (Add left right) maybeDest state =
    generateBinaryOp left right "+" maybeDest state
-- Subtração
generateExpr (Sub left right) maybeDest state =
    generateBinaryOp left right "-" maybeDest state
-- Multiplicação
generateExpr (Mul left right) maybeDest state =
    generateBinaryOp left right "*" maybeDest state
-- Divisão
generateExpr (Div left right) maybeDest state =
    generateBinaryOp left right "/" maybeDest state
-- Negação
generateExpr (Not expr) maybeDest state =
    let (exprTAC, exprTemp, state1) = generateExpr expr Nothing state
        (temp, state2) = case maybeDest of
            Just dest -> (dest, state1)
            Nothing   -> newTemp state1
    in (exprTAC ++ [TACUnaryOp temp "!" exprTemp], temp, state2)
-- AND
generateExpr (And left right) maybeDest state =
    generateBinaryOp left right "&&" maybeDest state
-- OR
generateExpr (Or left right) maybeDest state =
    generateBinaryOp left right "||" maybeDest state
-- Igual
generateExpr (Equal left right) maybeDest state =
    generateBinaryOp left right "==" maybeDest state
-- Diferente
generateExpr (NotEqual left right) maybeDest state =
    generateBinaryOp left right "!=" maybeDest state
-- Menor
generateExpr (LessThan left right) maybeDest state =
    generateBinaryOp left right "<" maybeDest state
-- Menor ou igual
generateExpr (LessEqual left right) maybeDest state =
    generateBinaryOp left right "<=" maybeDest state
-- Maior
generateExpr (GreaterThan left right) maybeDest state =
    generateBinaryOp left right ">" maybeDest state
-- Maior ou igual
generateExpr (GreaterEqual left right) maybeDest state =
    generateBinaryOp left right ">=" maybeDest state
-- Para strings
generateExpr (Str str) _ state =
    let temp = "str" ++ show (tempCounter state)    -- Criar um nome temporário para a string (ex. str0)
        state1 = state { tempCounter = tempCounter state + 1 }      -- Atualiza o estado, incrementando o contador de variáveis temporárias
    in ([TACAssign temp str], temp, state1)
-- Readln()
generateExpr ReadLn maybeDest state =
    let (temp, state1) = case maybeDest of
            Just dest -> (dest, state)
            Nothing   -> newTemp state
    in ([TACRead temp], temp, state1)


-- Função auxiliar para operações binárias
generateBinaryOp :: Exp -> Exp -> String -> Maybe String -> ICGState -> ([TAC], String, ICGState)
generateBinaryOp left right op maybeDest state =
    let (leftTAC, leftTemp, state1) = generateExpr left Nothing state
        (rightTAC, rightTemp, state2) = generateExpr right Nothing state1
        (temp, state3) = case maybeDest of
            Just dest -> (dest, state2) 
            Nothing   -> newTemp state2
    in (leftTAC ++ rightTAC ++ [TACBinOp temp leftTemp op rightTemp], temp, state3)


-- Função para converter comandos em TAC
generateStmt :: Exp -> ICGState -> ([TAC], ICGState)
-- Atribuições
generateStmt (Assign name expr) state =
    case expr of
        -- Numeros: converte o número numa string e gera uma TACAssign
        Num n ->
            let value = show (round n :: Int)
            in ([TACAssign name value], state)
        -- Strings: são atribuidas diretamente
        Str str ->
            let value = str
            in ([TACAssign name value], state)
        -- Booleanos: True é convertido para 1 e False para 0
        Bool b ->
            let value = if b then "1" else "0"
            in ([TACAssign name value], state)
        -- Para outras expressões:
        _ ->
            let (exprTAC, exprTemp, state1) = generateExpr expr (Just name) state
            in (exprTAC, state1)
-- Print(strings)
generateStmt (Print (Str str)) state =
    let (temp, state1) = newTemp state
    in ([TACAssign temp str, TACPrint temp], state1)
-- Println(strings)
generateStmt (PrintLn (Str str)) state =
    let (temp, state1) = newTemp state
    in ([TACAssign temp str, TACPrintLn temp], state1)
-- Print(outras expressões): gera o TAC para avaliar a expressão antes de imprimir
generateStmt (Print expr) state =
    let (exprTAC, exprTemp, state1) = generateExpr expr Nothing state
    in (exprTAC ++ [TACPrint exprTemp], state1)
-- Print(outras expressões): gera o TAC para avaliar a expressão antes de imprimir
generateStmt (PrintLn expr) state =
    let (exprTAC, exprTemp, state1) = generateExpr expr Nothing state
    in (exprTAC ++ [TACPrintLn exprTemp], state1)
-- Return
generateStmt (Return expr) state =
    let (exprTAC, exprTemp, state1) = generateExpr expr Nothing state
    in (exprTAC ++ [TACReturn exprTemp], state1)
-- While
generateStmt (While cond body) state =
    let (condTAC, condTemp, state1) = generateExpr cond Nothing state
        (bodyTAC, state2) = generateStmts body state1
        (labelStart, state3) = newLabel state2      -- cria labels para marcar o inicio e o fim do loop
        (labelEnd, state4) = newLabel state3
        condJump = TACCondJump condTemp "==" "1" labelStart     -- avalia a condição, e se verdadeira executa o resto
        jumpBack = TACJump labelStart
    in ([TACLabel labelStart] ++ condTAC ++ [condJump] ++ bodyTAC ++ [jumpBack, TACLabel labelEnd], state4)
-- If    
generateStmt (If cond trueBranch) state =
    let (condTAC, condTemp, state1) = generateExpr cond Nothing state
        (trueTAC, state2) = generateStmts trueBranch state1
        (labelEnd, state3) = newLabel state2            
        condJump = TACCondJump condTemp "==" "0" labelEnd       -- avalia a condição, se for falsa, salta para o fim do bloco
    in (condTAC ++ [condJump] ++ trueTAC ++ [TACLabel labelEnd], state3)
-- If-Else    
generateStmt (IfElse cond trueBranch falseBranch) state =
    let (condTAC, condTemp, state1) = generateExpr cond Nothing state
        (trueTAC, state2) = generateStmts trueBranch state1         -- avalia a condição, se for verdadeira executa trueBranch
        (falseTAC, state3) = generateStmts falseBranch state2       -- avalia a condição, se for falsa executa falseBranch
        (labelTrue, state4) = newLabel state3
        (labelEnd, state5) = newLabel state4
        condJump = TACCondJump condTemp "==" "0" labelTrue 
    in (condTAC ++ [condJump] ++ trueTAC ++ [TACJump labelEnd, TACLabel labelTrue] ++ falseTAC ++ [TACLabel labelEnd], state5)


-- Função para gerar TAC para várias instruções
generateStmts :: [Exp] -> ICGState -> ([TAC], ICGState)
generateStmts [] state = ([], state)
generateStmts (stmt:stmts) state =
    let (stmtTAC, state1) = generateStmt stmt state
        (stmtsTAC, state2) = generateStmts stmts state1
    in (stmtTAC ++ stmtsTAC, state2)


-- Função que transforma a AST numa TAC
generateTAC :: Program -> [TAC]
generateTAC (Program (Main stmts)) =
    let (tac, _) = generateStmts stmts initialState
    in tac
