module Compiler where
--import BzoParser
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens










--This function will eventually become the compiler pipeline
--compileFile :: String -> BzoSyntax
--compileFile f = do
--  let lex = fileLexer (f ++ "\n")   -- adding a newline to the end corrects a bug in the REPL parser
--  bzoParser lex










compileFile' :: String -> String
compileFile' f =
  let out = fileLexer f "Bzo"
  in  case out of
    Left  err -> show err
    Right tks -> show tks
