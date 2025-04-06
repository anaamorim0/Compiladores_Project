module Main where

import Lexer
import Parser

main :: IO ()
main = do
  txt <- getContents
  print (parse $ alexScanTokens txt)