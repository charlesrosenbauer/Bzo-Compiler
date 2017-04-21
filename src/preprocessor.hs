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










data BzoPrepPattern
  = PatStr String
  | PatInt Integer
  | PatFlt Double
  | PatId  String
  | PatMId String
  | PatTId String
  | PatBId String
  | PatBTy String
  | PatCmp [BzoPrepPattern]
  | PatPly [BzoPrepPattern]










mkPat_Str = PatStr ""
mkPat_Int = PatInt 0
mkPat_Flt = PatFlt 0.0
mkPat_Id  = PatId  ""
mkPat_MId = PatMId ""
mkPat_TId = PatTId ""
mkPat_BId = PatBId ""
mkPat_BTy = PatBTy ""
mkPat_Cmp = PatCmp []
mkPat_Ply = PatPly []










matchPattern :: BzoPrepPattern -> BzoSyntax -> Bool
matchPattern (PatStr s) (BzS_Str   p s') = True
matchPattern (PatInt i) (BzS_Int   p i') = True
matchPattern (PatFlt f) (BzS_Flt   p f') = True
matchPattern (PatId  i) (BzS_Id    p i') = True
matchPattern (PatMId i) (BzS_MId   p i') = True
matchPattern (PatTId i) (BzS_TyId  p i') = True
matchPattern (PatBId i) (BzS_BId   p i') = True
matchPattern (PatBTy i) (BzS_BTId  p i') = True
matchPattern (PatCmp x) (BzS_Cmpd  p x') = True
matchPattern (PatPly x) (BzS_Poly  p x') = True
matchPattern _          _                = False










maybeIf :: Bool -> a -> Maybe a
maybeIf True  x = Just x
maybeIf False x = Nothing










matchBCall0 :: String -> BzoSyntax -> Bool
matchBCall0 s expr =
  case expr of
    (BzS_Expr p [(BzS_BTId p' st)]) -> (st == s)
    (BzS_Expr p [(BzS_BId  p' st)]) -> (st == s)
    _                               -> False










matchBCall1 :: String -> BzoPrepPattern -> BzoSyntax -> Maybe BzoSyntax
matchBCall1 s p0 expr =
  case expr of
    (BzS_Expr p [x0, (BzS_BTId p' st)]) -> maybeIf ((st == s) && (matchPattern p0 x0)) x0
    (BzS_Expr p [x0, (BzS_BId  p' st)]) -> maybeIf ((st == s) && (matchPattern p0 x0)) x0
    _                                   -> Nothing










matchBCall2 :: String -> BzoSyntax -> BzoPrepPattern -> Maybe BzoSyntax
matchBCall2 s expr p1 =
  case expr of
    (BzS_Expr p [(BzS_BTId p' st), x1]) -> maybeIf ((st == s) && (matchPattern p1 x1)) x1
    (BzS_Expr p [(BzS_BId  p' st), x1]) -> maybeIf ((st == s) && (matchPattern p1 x1)) x1
    _                                   -> Nothing










matchBCall3 :: String -> BzoPrepPattern -> BzoSyntax -> BzoPrepPattern -> Maybe (BzoSyntax, BzoSyntax)
matchBCall3 s p0 expr p1 =
  case expr of
    (BzS_Expr p [x0, (BzS_BTId p' st), x1]) -> maybeIf ((st == s) && (matchPattern p0 x0) && (matchPattern p1 x1)) (x0, x1)
    (BzS_Expr p [x0, (BzS_BId  p' st), x1]) -> maybeIf ((st == s) && (matchPattern p0 x0) && (matchPattern p1 x1)) (x0, x1)
    _                                   -> Nothing








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
