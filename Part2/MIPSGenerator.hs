module MIPSGenerator where          -- Code Generator for MIPS

import ICG (TAC(..))                -- Importa a TAC do Intermediate Code Generator
import Data.List (nub, isPrefixOf, isSuffixOf)  -- Importa nub (remove elementos duplicados) e isPrefixOf (verifica se uma string começa com uma letra (neste caso) )
import System.IO (writeFile)        -- Importa a função para guardar strings em ficheiros

-- Função que gera código MIPS para uma TAC e guarda o resultado num ficheiro
generateMIPSFile :: [TAC] -> FilePath -> IO ()
generateMIPSFile tacs filename = do
    let mipsCode = unlines $ generateMIPS tacs
    writeFile filename mipsCode
    putStrLn $ "Codigo MIPS em " ++ filename

-- Função que converte uma TAC em código MIPS
generateMIPS :: [TAC] -> [String]
generateMIPS tacs =
    let variables = extractVariables tacs               -- extrai todas as variáveis usadas na TAC
        dataSection = generateDataSection variables     -- gera a secção .data do MIPS, onde são inicializadas as variáveis
        code = concatMap generate tacs               -- converte cada instrução TAC em MIPS
    in dataSection ++ initialConf ++ code ++ exitSyscall        -- combina as instruções do inicio, o código principal e as instruções de saída

-- Função que define as instruções de saída do programa
exitSyscall :: [String]
exitSyscall =
    [ "li $v0, 10"
    , "syscall"
    ]

-- Função que extrai todas as varávies usadas na TAC
extractVariables :: [TAC] -> [String]
extractVariables = nub . filter (not . isTemp) . concatMap extractFromTAC       -- Extrai as variaveis removendo as duplicadas e as temporarias
  where
    isTemp :: String -> Bool
    isTemp name = "t" `isPrefixOf` name         -- Verifica se o nome da variável começa com t, se começar é porque é temporária
    
    -- Extrair as variáveis
    extractFromTAC :: TAC -> [String]
    extractFromTAC (TACAssign dest _) = [dest]
    extractFromTAC (TACBinOp dest _ _ _) = [dest]
    extractFromTAC (TACLogicalOp dest _ _ _) = [dest]
    extractFromTAC (TACUnaryOp dest _ _) = [dest]
    extractFromTAC (TACRead dest) = [dest]
    extractFromTAC _ = []

-- Função com a secção .data
generateDataSection :: [String] -> [String]
generateDataSection vars =
    [ ".data" ] ++ map (\var -> var ++ ": .word 0") vars ++ ["newline: .asciiz \"\\n\""]

-- Função com a parte inicial do codigo MIPS
initialConf :: [String]
initialConf =
    [ ".text"
    , "_main:"
    ]

-- Função que converte cada instrução TAC para a correspondente em MIPS
generate :: TAC -> [String]
generate tac =
    case tac of
        -- Atribuições
        TACAssign dest value
            -- se value for uma variavel temporária (começa com 't') armazenamos o seu valor diretamente na destination
            | "t" `isPrefixOf` value -> [ "sw $" ++ value ++ ", " ++ dest ]
            -- Se o value for uma string (presença de aspas)
            | "\"" `isPrefixOf` value && "\"" `isSuffixOf` value ->
                let label = dest ++ "_str" 
                in [ ".data"                -- Adicionar a string na seção .data
                   , label ++ ": .asciiz " ++ value
                   , ".text"
                   , "la $t0, " ++ label   -- Carregar o endereço da string em $t0
                   , "sw $t0, " ++ dest    -- Armazenar o endereço em dest
                   ]
            -- se value é um númeiro, carregamos esse valor em $t0 e armazenamos-o em destination
            | otherwise -> [ "li $t0, " ++ value, "sw $t0, " ++ dest ]

        -- Operações binárias
        TACBinOp dest left op right ->
            let opCode = case op of
                    "+" -> "add"
                    "-" -> "sub"
                    "*" -> "mul"
                    "/" -> "div"
                    "==" -> "seq"
                    "!=" -> "sne"
                    "<"  -> "slt"
                    ">"  -> "sgt"
                    "<=" -> "sle"
                    ">=" -> "sge"
                    "&&" -> "and"
                    "||" -> "or"
            in [ "li $t0, " ++ left    -- load o operador da esquerda
               , "li $t0, " ++ right    -- load o operador da direita
               , opCode ++ " $t2, $t0, $t1"
               , "sw $t2, " ++ dest     -- armazenar o resultado
               ]

        -- Operações Unárias
        TACUnaryOp dest op value ->
            case op of
                "-" -> [ "li $t0, " ++ value
                       , "neg $t1, $t0"
                       , "sw $t1, " ++ dest
                       ]
                "!" -> [ "li $t0, " ++ value
                       , "seq $t1, $t0, $zero"      -- Compara $t0 com 0: $t1 = 1 se $t0 == 0, caso contrário $t1 = 0
                       , "sw $t1, " ++ dest
                       ]

        -- Saltos Condicionais
        TACCondJump x relop y label ->      -- if (x operador y) goto label
            let branchOp = case relop of
                    "==" -> "beq"
                    "!=" -> "bne"
                    ">"  -> "bgt"
                    "<"  -> "blt"
                    ">=" -> "bge"
                    "<=" -> "ble"
            in [ "lw $t0, " ++ x
               , "lw $t1, " ++ y
               , branchOp ++ " $t0, $t1, " ++ label
               ]

        -- Salto incondicional
        TACJump label -> [ "j " ++ label ]

        -- Label: 
        TACLabel label -> [ label ++ ":" ]

        -- Print(..)
        TACPrint value
            -- Verifica se é uma string
            | "\"" `isPrefixOf` value && "\"" `isSuffixOf` value ->
                let label = "_str" ++ value
                in [ ".data"
                   , label ++ ": .asciiz " ++ value
                   , ".text"
                   , "la $a0, " ++ label   -- Carrega o endereço da string
                   , "li $v0, 4"         
                   , "syscall"
                   ]
            -- Caso contrário, tratar como número
            | otherwise ->
                [ "li $v0, 1"       
                , "lw $a0, " ++ value  
                , "syscall"
                ]

        -- Println(..)
        TACPrintLn value
            -- Verifica se é uma string
            | "\"" `isPrefixOf` value && "\"" `isSuffixOf` value ->
                let label = "_str" ++ value
                in [ ".data"
                   , label ++ ": .asciiz " ++ value
                   , ".text"
                   , "la $a0, " ++ label   -- Carrega o endereço da string
                   , "li $v0, 4"         
                   , "syscall"
                   , "la $a0, newline"     -- Adiciona nova linha
                   , "li $v0, 4"
                   , "syscall"
                   ]
            -- Caso contrário, tratar como número
            | otherwise ->
                [ "li $v0, 1"
                , "lw $a0, " ++ value
                , "syscall"
                , "li $v0, 4"
                , "la $a0, newline"
                , "syscall"
                ]

        -- Readln(..)
        TACRead dest ->
            [ "li $v0, 5"
            , "syscall"
            , "sw $v0, " ++ dest
            ]

        -- Return
        TACReturn value ->
            [ "lw $v0, " ++ value
            , "jr $ra"      -- Saltar para a return adress
            ]
