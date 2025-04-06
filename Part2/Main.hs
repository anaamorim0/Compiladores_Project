import Lexer (alexScanTokens)
import Parser (parse)
import ICG (generateTAC)
import MIPSGenerator (generateMIPSFile)

main :: IO ()
main = do
    putStrLn "Insira um programa em kotlin: "
    input <- getContents
    let tokens = alexScanTokens input
    let ast = parse tokens
    putStr "AST: "
    print ast
    let tac = generateTAC ast
    putStrLn "TAC: "
    mapM_ print tac
    putStrLn ""
    generateMIPSFile tac "output.s"
