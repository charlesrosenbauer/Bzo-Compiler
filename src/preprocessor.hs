module BzoPreprocessor where
import BzoLexer
import BzoParser
import Control.Monad
import System.Environment
import System.IO hiding (try)










data BzoProjectSyntax
  = BzoProjectSyntax{
      fileAST  :: BzoSyntax,
      fileDeps :: [BzoProjectSyntax] }










loadAST :: String -> String -> Either [BzoErr] BzoSyntax
loadAST fname fcontent =
  let lexSyms = fileLexer fname fcontent
  case lexSyms of
    Left err -> [err]
    Right ts -> parseFile fname ts [parseCalls]










preprocessStage :: [FilePath] -> IO Either [BzoErr] BzoProjectSyntax
preprocessStage = do
