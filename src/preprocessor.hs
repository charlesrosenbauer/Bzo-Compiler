module BzoPreprocessor where
import BzoLexer
import BzoParser
import BzoSyntax
import BzoTypes
import Data.Map.Strict
import Control.Monad
import System.Environment
import System.IO hiding (try)










data BzoFileData
  = BzoFileData{
      bfd_moduleName    :: String,
      bfd_filepath      :: FilePath,
      bfd_fileAST       :: BzoSyntax,
      bfd_fileImports   :: [String],
      bfd_fileLinks     :: [String],
      bfd_fileImporsAs  :: [(String, String)],
      bfd_fileLinksAs   :: [(String, String)]}










showBzoFileData :: BzoFileData -> String
showBzoFileData (BzoFileData mn pth ast imp lnk impa lnka) =
  "Module: " ++ mn ++ "\nPath: " ++ pth ++
    "\nImports: " ++ (show imp) ++ "\nAliased Imports: " ++ (show impa) ++
    "\nLinks: "   ++ (show lnk) ++ "\nAliased Links: "   ++ (show lnka) ++
    "\nAST:\n"     ++ (show ast)
instance Show BzoFileData where show = showBzoFileData










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










verifyAST :: Either [BzoErr] (Bool, BzoSyntax, BzoFileData) -> Either [BzoErr] (Bool, BzoSyntax, BzoFileData)
verifyAST (Left errs) = Left errs




verifyAST (Right (b,     (BzS_Calls p []),     (BzoFileData mn path ast imp lnk impa lnka))) = Right (b, ast, (BzoFileData mn path ast imp lnk impa lnka))




verifyAST (Right (False, ast, (BzoFileData "" path (BzS_Calls p (x:xs)) [] [] [] []))) =
      case (matchBCall1 "$Module" mkPat_TId x) of
        Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData (sid syn) path (BzS_Calls p xs) [] [] [] []))
        _        -> Left [PrepErr p "Illegal formatting. $Module must be defined at beginning of file.\n"]




verifyAST (Right (False, ast, (BzoFileData mn path (BzS_Calls p (x : xs)) imp lnk impa lnka))) =
      case (matchBCall1 "$import" mkPat_TId x) of
        Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path (BzS_Calls p xs) ([sid syn] ++ imp) lnk impa lnka))
        _        ->
          case (matchBCall1 "$link" mkPat_TId x) of
            Just syn -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path (BzS_Calls p xs) imp ([sid syn] ++ lnk) impa lnka))
            _        ->
              case (matchBCall3 "$importAs" mkPat_TId x mkPat_TId) of
                Just (a, b) -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path (BzS_Calls p xs) imp lnk ([(sid a, sid b)] ++ impa) lnka))
                _           ->
                  case (matchBCall3 "$linkAs" mkPat_TId x mkPat_TId) of
                    Just (a, b) -> verifyAST $ Right (False, (BzS_Calls p xs), (BzoFileData mn path (BzS_Calls p xs) imp lnk impa ([(sid a, sid b)] ++ lnka)))
                    _           -> verifyAST $ Right (True,  ast,              (BzoFileData mn path (BzS_Calls p (x : xs)) imp lnk impa lnka))




verifyAST (Right (True, (BzS_Calls p (x : xs)), (BzoFileData mn path ast imp lnk impa lnka))) =
      case (matchBCall1 "$import" mkPat_TId x) of
        Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $import must be at the beginning of file.\n"]
        _        ->
          case (matchBCall1 "$link" mkPat_TId x) of
            Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $link must be at the beginning of file.\n"]
            _        ->
              case (matchBCall3 "$importAs" mkPat_TId x mkPat_TId) of
                Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $importAs must be at the beginning of file.\n"]
                _        ->
                  case (matchBCall3 "$linkAs" mkPat_TId x mkPat_TId) of
                    Just syn -> Left [PrepErr (pos x) "Illegal formatting. All instances of $linkAs must be at the beginning of file.\n"]
                    _        ->
                      case (matchBCall1 "$Module" mkPat_TId x) of
                        Just syn -> Left [PrepErr (pos x) "Illegal formatting. Only one instance of $Module per file.\n"]
                        _        -> verifyAST $ Right (True, (BzS_Calls p xs), (BzoFileData mn path ast imp lnk impa lnka))









{- Heavy Construction Zone!!
loadFullProject :: FilePath -> CfgSyntax -> [BzoFileData] -> IO (Either [BzoErr] [BzoFileData])
loadFullProject path (LibLines p ls) ds =
  let libpaths  = scanl (\m x -> insert (libName x) (libPath (x ++ path)) m) empty ls      -- produce Map of library names to library paths
      linkNeeds = concatMap (\(BzoFileData mn fp sn fi fl ia la) -> fl ++ (map fst la)) ls -- get list of libraries to import
  in
-}
