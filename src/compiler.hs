module Compiler where
import BzoParser
import BzoParserRules
import BzoTypes
import BzoLexer
import BzoSyntax
import BzoTokens










data BzoSettings
  = BzoSettings {
    importedFiles :: [(FilePath, String)],
    linkedFiles   :: [(FilePath, String)]}









compileFile :: String -> String -> String
compileFile name f =
  let out = fileLexer f name
  in  case out of
    Left  err -> show err
    Right tks ->
      show $ parseFile name tks [parseCalls]
