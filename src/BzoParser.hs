module BzoParser where
import BzoTypes
import BzoParserRules
import HigherOrder









{-
data ParseItem
  = PI_Token{ piTok :: !BzoToken  }
  | PI_BzSyn{ piSyn :: !BzoSyntax }
  | PI_CPXS { piSyns:: ![BzoSyntax] }
  | PI_CPX  { piSyn :: !BzoSyntax }
  | PI_PLXS { piSyns:: ![BzoSyntax] }
  | PI_PLX  { piSyn :: !BzoSyntax }
  | PI_TX   { piSyn :: !BzoSyntax }
  | PI_RX   { piSyn :: !BzoSyntax }
  | PI_MS   { piSyn :: !BzoSyntax }
  | PI_MX   { piSyn :: !BzoSyntax }
  | PI_BKX  { piSyn :: !BzoSyntax }
  | PI_Exs  { piSyns:: ![BzoSyntax] }
  | PI_Err  { piErr :: !BzoErr }
  | PI_SOF
  | PI_Cfg  { piCfg :: !CfgSyntax }
  deriving Show
-}









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









{-
incASTHelp0 :: (BzoSyntax -> [BzoErr]) -> MockParseItem -> BzoSyntax -> [BzoErr]
incASTHelp0 errFn mock sn = applyIfE errFn (\x -> []) sn (matchSyntax mock sn)









includesASTItem :: (BzoSyntax -> [BzoErr]) -> MockParseItem -> BzoSyntax -> [BzoErr]
includesASTItem errFn mock sn@(BzS_ArrayObj    p o x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock o) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_CurryObj    p o x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock o) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_Calls         p x) = (incASTHelp0 errFn mock sn) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_FunDef  p i x e d) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock i) ++ (includesASTItem errFn mock e) ++ (includesASTItem errFn mock d)
includesASTItem errFn mock sn@(BzS_TypDef    p i x d) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock i) ++ (includesASTItem errFn mock d)
includesASTItem errFn mock sn@(BzS_FnTypeDef   p x d) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock d)
includesASTItem errFn mock sn@(BzS_Cmpd          p x) = (incASTHelp0 errFn mock sn) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_Poly          p x) = (incASTHelp0 errFn mock sn) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_Expr          p x) = (incASTHelp0 errFn mock sn) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_Block         p x) = (incASTHelp0 errFn mock sn) ++ (concatMap (includesASTItem errFn mock) x)
includesASTItem errFn mock sn@(BzS_FilterObj   p x l) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x) ++ (concatMap (includesASTItem errFn mock) l)
includesASTItem errFn mock sn@(BzS_Lambda      p x d) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x) ++ (includesASTItem errFn mock d)
includesASTItem errFn mock sn@(BzS_FnTy        p i o) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock i) ++ (includesASTItem errFn mock o)
includesASTItem errFn mock sn@(BzS_Box           p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_Filter        p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_MapObj        p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_ArrExprMod    p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_Curry         p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn = (incASTHelp0 errFn mock sn)
-}









fuseNameObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseNameObj (BzS_TyId p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExTypObj p0 i0 i1)]
fuseNameObj (BzS_Id   p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExFunObj p0 i0 i1)]
fuseNameObj a                b                     = [a, b]









{-
fuseCurryObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseCurryObj sn@(BzS_Curry p0 (BzS_Curry p1 o)) x          = [sn, x]
fuseCurryObj sn@(BzS_Box        p1   x ) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj sn@(BzS_Cmpd       p1   xs) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj sn@(BzS_Poly       p1   xs) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj sn@(BzS_TyId       p1   x ) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj sn@(BzS_Id         p1   x ) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj sn@(BzS_MId        p1   x ) (BzS_Curry p0 o0) = [(BzS_CurryObj p0 sn [o0])]
fuseCurryObj (BzS_CurryObj   p1 o os)    (BzS_Curry p0 o0) = [(BzS_CurryObj p0 o  (o0:os))]
fuseCurryObj a                 b                           = [a, b]










fuseFilterObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseFilterObj sn@(BzS_TyId  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_Id    p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_Cmpd  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_Poly  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_TyVar p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_Box   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_MId   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn [f])]
fuseFilterObj sn@(BzS_FilterObj p0 x fs) (BzS_Filter p1 f) = [(BzS_FilterObj p0 x (fs ++ [f]))]
fuseFilterObj a                b                     = [a, b]










-- | Assumes that the syntax stream is reversed
fuseArrayObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseArrayObj sn@(BzS_ArrayObj  p0 o x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 o $ x ++ [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_ArrayObj  p0 o x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 o $ x ++ [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_TyId        p0 i) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_TyId        p0 i) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_Id          p0 i) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_Id          p0 i) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_Cmpd        p0 x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_Cmpd        p0 x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_Poly        p0 x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_Poly        p0 x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_TyVar       p0 x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_TyVar       p0 x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_Box         p0 x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_Box         p0 x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_BTId        p0 x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_BTId        p0 x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_FilterObj p0 o x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_FilterObj p0 o x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_ExTypObj  p0 o x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_ExTypObj  p0 o x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj sn@(BzS_ExFunObj  p0 o x) (BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [(BzS_ArrGnObj p1  )])]
fuseArrayObj sn@(BzS_ExFunObj  p0 o x) (BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [(BzS_ArrSzObj p1 s)])]
fuseArrayObj a                       b                       = [a, b]










fuseMapObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseMapObj    (BzS_Box    p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0  x)]
fuseMapObj sn@(BzS_Id     p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_MId    p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Lambda p0 x d) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Cmpd   p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Poly   p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj a                      b               = [a, b]










-- Quick Hack to fix a problem. Not efficient. Needs a better revision later.
identityPass :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
identityPass a b = [a, b]










passHelp0 :: ([BzoSyntax] -> [BzoSyntax]) -> (BzoSyntax -> BzoSyntax -> [BzoSyntax]) -> BzoSyntax -> BzoSyntax
passHelp0 xf f x = simplifyASTPass xf f x










passHelp1 :: ([BzoSyntax] -> [BzoSyntax]) -> (BzoSyntax -> BzoSyntax -> [BzoSyntax]) -> [BzoSyntax] -> [BzoSyntax]
passHelp1 xf f x = map (simplifyASTPass xf f) (simplifyList (xf x) f)










simplifyASTPass :: ([BzoSyntax] -> [BzoSyntax]) -> (BzoSyntax -> BzoSyntax -> [BzoSyntax]) -> BzoSyntax -> BzoSyntax
simplifyASTPass xf f (BzS_ArrayObj    p o x) = (BzS_ArrayObj   p (passHelp0 xf f o) (passHelp1 xf f x))
simplifyASTPass xf f (BzS_CurryObj    p o x) = (BzS_CurryObj   p (passHelp0 xf f o) (passHelp1 xf f x))
simplifyASTPass xf f (BzS_Calls         p x) = (BzS_Calls      p (passHelp1 xf f x))
simplifyASTPass xf f (BzS_FunDef  p i x e d) = (BzS_FunDef     p (passHelp0 xf f i) x (passHelp0 xf f e) (passHelp0 xf f d))
simplifyASTPass xf f (BzS_TypDef    p i x d) = (BzS_TypDef     p (passHelp0 xf f i) x (passHelp0 xf f d))
simplifyASTPass xf f (BzS_FnTypeDef   p x d) = (BzS_FnTypeDef  p x (passHelp0 xf f d))
simplifyASTPass xf f (BzS_Cmpd          p x) = (BzS_Cmpd       p (passHelp1 xf f x))
simplifyASTPass xf f (BzS_Poly          p x) = (BzS_Poly       p (passHelp1 xf f x))
simplifyASTPass xf f (BzS_Expr          p x) = (BzS_Expr       p (passHelp1 xf f x))
simplifyASTPass xf f (BzS_Block         p x) = (BzS_Block      p (passHelp1 xf f x))
simplifyASTPass xf f (BzS_FilterObj   p x l) = (BzS_FilterObj  p (passHelp0 xf f x) (passHelp1 xf f l))
simplifyASTPass xf f (BzS_Lambda      p x d) = (BzS_Lambda     p (passHelp0 xf f x) (passHelp0 xf f d))
simplifyASTPass xf f (BzS_FnTy        p i o) = (BzS_FnTy       p (passHelp0 xf f i) (passHelp0 xf f o))
simplifyASTPass xf f (BzS_Box           p x) = (BzS_Box        p (passHelp0 xf f x))
simplifyASTPass xf f (BzS_Curry         p x) = (BzS_Curry      p (passHelp0 xf f x))
simplifyASTPass xf f (BzS_Filter        p x) = (BzS_Filter     p (passHelp0 xf f x))
simplifyASTPass xf f (BzS_MapObj        p x) = (BzS_MapObj     p (passHelp0 xf f x))
simplifyASTPass xf f (BzS_ArrExprMod    p x) = (BzS_ArrExprMod p (passHelp0 xf f x))
simplifyASTPass xf f sn@(BzS_ArrGenMod    _) = sn
simplifyASTPass xf f sn@(BzS_ArrSzMod   _ _) = sn
simplifyASTPass xf f sn@(BzS_ExTypObj _ _ _) = sn
simplifyASTPass xf f sn@(BzS_ExFunObj _ _ _) = sn
simplifyASTPass xf f sn@(BzS_Flt        _ _) = sn
simplifyASTPass xf f sn@(BzS_Str        _ _) = sn
simplifyASTPass xf f sn@(BzS_Int        _ _) = sn
simplifyASTPass xf f sn@(BzS_Id         _ _) = sn
simplifyASTPass xf f sn@(BzS_BId        _ _) = sn
simplifyASTPass xf f sn@(BzS_BTId       _ _) = sn
simplifyASTPass xf f sn@(BzS_TyId       _ _) = sn
simplifyASTPass xf f sn@(BzS_TyVar      _ _) = sn
simplifyASTPass xf f sn@(BzS_MId        _ _) = sn
simplifyASTPass xf f sn@(BzS_Wildcard     _) = sn
simplifyASTPass xf f sn@(BzS_Nil          _) = sn
simplifyASTPass xf f sn@(BzS_Namespace  _ _) = sn
simplifyASTPass xf f sn@(BzS_MapMod       _) = sn
simplifyASTPass xf f sn@(BzS_Undefined     ) = sn
simplifyASTPass xf f sn@(BzS_ArrSzObj   _ _) = sn
simplifyASTPass xf f sn@(BzS_ArrGnObj     _) = sn









{-
simplifyAST :: BzoSyntax -> Either [BzoErr] BzoSyntax
simplifyAST ast =
  let pass0 = simplifyASTPass id fuseNameObj   ast
      pass1 = simplifyASTPass id fuseFilterObj pass0
      pass2 = simplifyASTPass reverse identityPass $ simplifyASTPass id fuseCurryObj $simplifyASTPass reverse fuseArrayObj pass1
      pass3 = simplifyASTPass id fuseMapObj pass2
      errs0 = includesASTItem (\sn -> [ParseErr (pos sn) "Unexpected Namespace Indicator\n"]) MP_Name   pass3
      errs1 = includesASTItem (\sn -> [ParseErr (pos sn) "Unexpected Filter Indicator\n"   ]) MP_Filt   pass3
      errs2 = includesASTItem (\sn -> [ParseErr (pos sn) "Unexpected Map Indicator\n"      ]) MP_MapMod pass3
      errs3 = includesASTItem (\sn -> [ParseErr (pos sn) "Unexpected Array Indicator\n"    ]) MP_AGMod  pass3
      errs4 = includesASTItem (\sn -> [ParseErr (pos sn) "Unexpected Array Indicator\n"    ]) MP_ASMod  pass3
      errs5 = includesASTItem (\sn -> [ParseErr (pos sn) "Invalid Array Indicator\n"       ]) MP_AXMod  pass3
      errs6 = includesASTItem (\sn -> [ParseErr (pos sn) "Invalid Use of Curry Operator\n" ]) MP_Curry  pass3
      errs  = errs0 ++ errs1 ++ errs2 ++ errs3 ++ errs4 ++ errs5 ++ errs6
  in case errs of
        [] -> Right pass3
        er -> Left  er
-}-}









parseFile :: String -> [BzoToken] -> Either [BzoErr] BzoSyntax
parseFile fname tks =
  let bracketErrs = bracketCheck fname tks
      parseOut    = parserIter fname (map (\t -> BzS_Token (spos t) t) tks) []
  in case (bracketErrs, parseOut) of
      (Just errs,        _ ) -> Left errs
      (Nothing  , Left errs) -> Left errs
      (Nothing  , Right ast) -> Right ast
          {-case (simplifyAST ast) of
            Left errs -> Left errs
            Right out -> Right out-}
