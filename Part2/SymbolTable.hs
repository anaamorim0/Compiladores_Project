module SymbolTable where

import qualified Data.Map as Map

-- Tipos da linguagem Kotlin (Int, Bool, de Função)
data Type = TyInt | TyBool | TyFun [Type] Type
        deriving (Show, Eq)

-- Informações associadas a um identificador
data SymbolInfo = SymbolInfo
    { symbolType :: Type        -- o tipo do identificador
    , isMutable :: Bool         -- se a variável é mutável ou imutável
    , memoryAddress :: String   -- o endereço de memória associado ao identificador
    } deriving (Show)

-- A tabela de símbolos (um mapeamento de nomes de identificadores para as suas informações)
type SymbolTable = Map.Map String SymbolInfo

-- Pilha de tabelas de tabelas de símbolos
type ScopedSymbolTable = [SymbolTable]

-- Cria uma tabela de símbolos inicial, com um único âmbito vazio
emptySymbolTable :: ScopedSymbolTable
emptySymbolTable = [Map.empty]

-- Insere um identificador no âmbito atual (o primeiro elemento da pilha)
insertSymbol :: String -> SymbolInfo -> ScopedSymbolTable -> ScopedSymbolTable
insertSymbol name info (scope:scopes) =
    Map.insert name info scope : scopes
insertSymbol _ _ [] = error "Não existe um âmbito disponível"

-- Procura um identificador começando pelo âmbito atual
lookupSymbol :: String -> ScopedSymbolTable -> Maybe SymbolInfo
lookupSymbol name [] = Nothing
lookupSymbol name (scope:scopes) =
    case Map.lookup name scope of
        Just info -> Just info
        Nothing   -> lookupSymbol name scopes

-- Abre um novo âmbito, adicionando um Map.empty no top da pilha de âmbitos
openScope :: ScopedSymbolTable -> ScopedSymbolTable
openScope scopes = Map.empty : scopes

-- Fecha o âmbito mais interno, removendo o primeiro elemento da pilha
closeScope :: ScopedSymbolTable -> ScopedSymbolTable
closeScope (_:scopes) = scopes
closeScope [] = error "Não existe nenhum âmbito para fechar"