module Compiler where
import BzoParser
import BzoTypes
import BzoLexer
import BzoSyntax










--This function will eventually become the compiler pipeline
compileFile :: String -> BzoSyntax
compileFile f = do
  let lex = fileLexer (f ++ "\n")   -- adding a newline to the end corrects a bug in the REPL parser
  bzoParser lex










compileFile' :: String -> [BzoToken]
compileFile' f = fileLexer f
