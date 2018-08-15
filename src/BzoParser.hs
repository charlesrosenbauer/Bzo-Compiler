module BzoParser where
import BzoTypes
import BzoParserRules
import HigherOrder










isBracket :: BzoToken -> Bool
isBracket (TkStartDo  _) = True
isBracket (TkStartTup _) = True
isBracket (TkStartDat _) = True
isBracket (TkEndDo    _) = True
isBracket (TkEndTup   _) = True
isBracket (TkEndDat   _) = True
isBracket _              = False










-- Probably could use a monad, but that's too much work
combineBracketChecks :: (String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> (String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
combineBracketChecks f0 f1 fname tks =
  let a = case (f0 fname tks) of
            (Just errs, tks') -> (errs, f1 fname tks')
            (Nothing  , tks') -> ([]  , f1 fname tks')
  in case a of
      ([],      (Nothing   , tks')) -> (Nothing              , tks')
      (errs0,   (Just errs1, tks')) -> (Just (errs0 ++ errs1), tks')
      (errs0,   (Nothing   , tks')) -> (Just errs0           , tks')











recoverBracketCheck :: (String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> String -> [BzoErr] -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
recoverBracketCheck fn fname errs tks =
  let out = fn fname tks
  in case out of
    (Nothing   , tks') -> (Just errs           , tks')
    (Just errs0, tks') -> (Just (errs ++ errs0), tks')










bracketCheck_Tuple :: String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Tuple fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndTup   ps) : tks) = (Nothing, tks)
bracketCheck_Tuple fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data  bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Tuple fname [ParseErr ps "Invalid placement of ']' inside Tuple"] tks
bracketCheck_Tuple fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Tuple fname tks
bracketCheck_Tuple fname ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Tuple fname [ParseErr ps "Invalid placement of '}' inside Tuple"] tks
bracketCheck_Tuple fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) "Mismatched parentheses"], [])
bracketCheck_Tuple fname (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Tuple function"], tks)










bracketCheck_Data :: String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Data fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Data fname tks
bracketCheck_Data fname ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Data fname [ParseErr ps "Invalid placement of ')' inside Array Modifier"] tks
bracketCheck_Data fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Data  fname tks
bracketCheck_Data fname ((TkEndDat   ps) : tks) = (Nothing, tks)
bracketCheck_Data fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Data fname tks
bracketCheck_Data fname ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Data fname [ParseErr ps "Invalid placement of '}' inside Array Modifier"] tks
bracketCheck_Data fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) "Mismatched Square Brackets"], [])
bracketCheck_Data fname (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Data function"], tks)










bracketCheck_Block :: String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Block fname ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Block fname tks
bracketCheck_Block fname ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Block fname [ParseErr ps "Invalid placement of ')' inside Block"] tks
bracketCheck_Block fname ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Block  fname tks
bracketCheck_Block fname ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Block fname [ParseErr ps "Invalid placement of ']' inside Block"] tks
bracketCheck_Block fname ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Block fname tks
bracketCheck_Block fname ((TkEndDo    ps) : tks) = (Nothing, tks)
bracketCheck_Block fname ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 fname) "Mismatched Braces"], [])
bracketCheck_Block fname (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Block function"], tks)









bracketCheckFn :: String -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheckFn fname (t : ts) = case t of
          (TkStartTup ps) -> bracketCheck_Tuple fname ts
          (TkStartDat ps) -> bracketCheck_Data  fname ts
          (TkStartDo  ps) -> bracketCheck_Block fname ts
          (TkEndTup   ps) -> (Just $ [ParseErr ps "Mismatched parentheses!"], ts)
          (TkEndDat   ps) -> (Just $ [ParseErr ps "Mismatched square brackets!"], ts)
          (TkEndDo    ps) -> (Just $ [ParseErr ps "Mismatched braces!"], ts)
          tk              -> (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheckFn function"], ts)










bracketCheck :: String -> [BzoToken] -> Maybe [BzoErr]
bracketCheck fname tks =
  let tks' = filter isBracket tks
  in checkIter tks' (bracketCheckFn fname)
    where checkIter x f = case x of
            [] -> Nothing
            _  -> case (f x) of
                    (Nothing, []) -> Nothing
                    (Nothing, ts) -> checkIter ts f
                    (Just es, []) -> Just es
                    (Just es, ts) -> Just $ maybeMerge es (checkIter ts f)










fuseNameObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseNameObj (BzS_TyId p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExTypObj p0 i0 i1)]
fuseNameObj (BzS_Id   p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExFunObj p0 i0 i1)]
fuseNameObj a                b                     = [a, b]










parseFile :: String -> [BzoToken] -> Either [BzoErr] BzoSyntax
parseFile fname tks =
  let bracketErrs = bracketCheck fname tks
      parseOut    = parserIter fname (map (\t -> BzS_Token (spos t) t) tks) []
  in case (bracketErrs, parseOut) of
      (Just errs,        _ ) -> Left errs
      (Nothing  , Left errs) -> Left errs
      (Nothing  , Right ast) -> Right ast
