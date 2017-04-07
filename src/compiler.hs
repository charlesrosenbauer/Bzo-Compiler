module Compiler where
import BzoParser
import BzoParserRules
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens










--This function will eventually become the compiler pipeline
--compileFile :: String -> BzoSyntax
--compileFile f = do
--  let lex = fileLexer (f ++ "\n")   -- adding a newline to the end corrects a bug in the REPL parser
--  bzoParser lex










compileFile' :: String -> String -> String
compileFile' name f =
  let out = fileLexer f name
  in  case out of
    Left  err -> show err
    Right tks ->
      show $ parseFile name tks [parseCalls]
