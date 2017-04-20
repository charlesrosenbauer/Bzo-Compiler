module BzoPreprocessor where
import BzoLexer
import BzoParser
import BzoSyntax
import BzoTypes
import Control.Monad
import System.Environment
import Compiler
import System.IO hiding (try)










data BzoProjectSyntax
  = BzoProjectSyntax{
      fileAST  :: BzoSyntax,
      fileDeps :: [BzoProjectSyntax] }










getModuleName :: BzoSyntax -> Either BzoErr String    -- Should probably find a more general way to accomplish this.
getModuleName ast =
  case ast of
    (BzS_Calls pos (c : cs)) ->
      case c of
        (BzS_Expr p [(BzS_TyId _ tid), (BzS_BTId _ "$Module")]) -> Right tid
        _                                                       -> Left $ PrepErr "Invalid Formatting in File: expected module definition"
    expr                     -> Left  $ PrepErr "Invalid Formatting in File"










--preprocessStage :: [FilePath] -> IO Either [BzoErr] BzoProjectSyntax
--preprocessStage = do
