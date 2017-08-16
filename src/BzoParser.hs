module BzoParser where
import BzoTypes
import BzoSyntax
import HigherOrder










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










getPIPos :: ParseItem -> BzoPos
getPIPos (PI_Token         tk) = spos tk
getPIPos (PI_SOF             ) = BzoPos 0 0 "Empty File"
getPIPos (PI_Err           er) = position er
getPIPos (PI_CPXS          ps) = pos $ head ps
getPIPos (PI_PLXS          ps) = pos $ head ps
getPIPos (PI_Exs           ps) = pos $ head ps
getPIPos (PI_Cfg           ps) = cpos ps
getPIPos x                     = pos $ piSyn x










data MockParseItem
  = MP_FunDef
  | MP_TypDef
  | MP_FnTypeDef
  | MP_Lambda
  | MP_Int
  | MP_Flt
  | MP_Str
  | MP_Id
  | MP_TId
  | MP_MId
  | MP_TyVr
  | MP_BId
  | MP_BTId
  | MP_Name
  | MP_Poly
  | MP_Cmpd
  | MP_FnTy
  | MP_Curry
  | MP_Blck
  | MP_Expr
  | MP_Calls
  | MP_Wild
  | MP_Undef
  | MP_Tk !BzoToken
  | MP_Tkn
  | MP_Box
  | MP_Cpxs
  | MP_Plxs
  | MP_Cpx
  | MP_Plx
  | MP_Tpx
  | MP_Tx
  | MP_Rx
  | MP_Tpxs
  | MP_Filt
  | MP_AGMod
  | MP_ASMod
  | MP_AXMod
  | MP_Any
  | MP_Parse
  | MP_Item
  | MP_Tup
  | MP_Typ
  | MP_Mod
  | MP_Vr
  | MP_Def
  | MP_Mx
  | MP_Ms
  | MP_Bkx
  | MP_Exs
  | MP_SOF
  | MP_CallItem
  | MP_Cfg_Line
  | MP_Cfg_Lines
  | MP_ExTypObj
  | MP_ExFunObj
  | MP_ArrayObj
  | MP_FilterObj
  | MP_CurryObj
  | MP_MapObj
  | MP_MapMod
  | MP_ArrSzObj
  | MP_ArrGnObj










mockPos = BzoPos 0 0 ""
mtk_StartTup  = MP_Tk $ TkStartTup  mockPos
mtk_EndTup    = MP_Tk $ TkEndTup    mockPos
mtk_StartDat  = MP_Tk $ TkStartDat  mockPos
mtk_EndDat    = MP_Tk $ TkEndDat    mockPos
mtk_StartDo   = MP_Tk $ TkStartDo   mockPos
mtk_EndDo     = MP_Tk $ TkEndDo     mockPos
mtk_SepExpr   = MP_Tk $ TkSepExpr   mockPos
mtk_SepPoly   = MP_Tk $ TkSepPoly   mockPos
mtk_CurrySym  = MP_Tk $ TkCurrySym  mockPos
mtk_FilterSym = MP_Tk $ TkFilterSym mockPos
mtk_LamdaSym  = MP_Tk $ TkLambdaSym mockPos
mtk_Reference = MP_Tk $ TkReference mockPos
mtk_Wildcard  = MP_Tk $ TkWildcard  mockPos
mtk_Define    = MP_Tk $ TkDefine    mockPos
mtk_FnSym     = MP_Tk $ TkFnSym     mockPos
mtk_TupEmpt   = MP_Tk $ TkTupEmpt   mockPos
mtk_ArrGnrl   = MP_Tk $ TkArrGnrl   mockPos
mtk_ArrMod    = MP_Tk $ TkArrMod    mockPos
mtk_Newline   = MP_Tk $ TkNewline   mockPos
mtk_Int       = MP_Tk $ TkInt       mockPos 0
mtk_Flt       = MP_Tk $ TkFlt       mockPos 0.0
mtk_Str       = MP_Tk $ TkStr       mockPos "This is a Mock String"
mtk_Id        = MP_Tk $ TkId        mockPos "this-is-a-Mock-Identifier"
mtk_TypeId    = MP_Tk $ TkTypeId    mockPos "This-is-a-Mock-Type-Identifier"
mtk_MutId     = MP_Tk $ TkMutId     mockPos "~this-is-a-Mock-Mutable-Identifier"
mtk_Builtin   = MP_Tk $ TkBuiltin   mockPos "$this-is-a-Mock-Builtin"
mtk_BIType    = MP_Tk $ TkBIType    mockPos "$This-is-a-Mock-Builtin-Type"
mtk_TyVar     = MP_Tk $ TkTyVar     mockPos "This-is-a-Mock-Type-Variable'"
mtk_Nil       = MP_Tk $ TkNil











matchParseItem :: MockParseItem -> ParseItem -> Bool
matchParseItem (MP_SOF  )(PI_SOF     ) = True
matchParseItem (MP_Parse)(PI_Token tk) = False
matchParseItem (MP_Parse) _            = True
matchParseItem (MP_Any  ) _            = True
matchParseItem (MP_Tk t) (PI_Token tk) = matchBzoToken t tk
matchParseItem (MP_Tkn ) (PI_Token tk) = True
matchParseItem (MP_Cpx ) (PI_CPX   x ) = True
matchParseItem (MP_Cpxs) (PI_CPXS  xs) = True
matchParseItem (MP_Plx ) (PI_PLX   x ) = True
matchParseItem (MP_Plxs) (PI_PLXS  xs) = True
matchParseItem (MP_Tx  ) (PI_TX    x ) = True
matchParseItem (MP_Rx  ) (PI_RX    x ) = True
matchParseItem (MP_Tpx ) (PI_PLX   x ) = True
matchParseItem (MP_Tpx ) (PI_CPX   x ) = True
matchParseItem (MP_Tpxs) (PI_PLXS  xs) = True
matchParseItem (MP_Tpxs) (PI_CPXS  xs) = True
matchParseItem (MP_Ms  ) (PI_MS    xs) = True
matchParseItem (MP_Bkx ) (PI_BKX   xs) = True
matchParseItem (MP_Exs ) (PI_Exs   xs) = True
matchParseItem (MP_CallItem)(PI_BzSyn (BzS_Expr         p x)) = True
matchParseItem (MP_CallItem)(PI_BzSyn (BzS_FnTypeDef  p i d)) = True
matchParseItem (MP_CallItem)(PI_BzSyn (BzS_TypDef   p i x q)) = True
matchParseItem (MP_CallItem)(PI_BzSyn (BzS_FunDef p x i q d)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_Cmpd       p x)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_Poly       p x)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_Box        p x)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_Id         p i)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_MId        p i)) = True
matchParseItem (MP_Vr  ) (PI_BzSyn (BzS_TyVar      p i)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Cmpd       p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_TyVar      p i)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Poly       p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Box        p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_TyId       p i)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_BTId       p i)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Nil        p  )) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Wildcard   p  )) = True
matchParseItem (MP_Typ ) (PI_MX                      x ) = True
matchParseItem (MP_Mod ) (PI_BzSyn (BzS_ArrGenMod  p  )) = True
matchParseItem (MP_Mod ) (PI_BzSyn (BzS_ArrSzMod   p s)) = True
matchParseItem (MP_Mod ) (PI_BzSyn (BzS_ArrExprMod p x)) = True
matchParseItem (MP_Def ) (PI_BzSyn (BzS_Expr       p x)) = True
matchParseItem (MP_Def ) (PI_BzSyn (BzS_Block      p x)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Id         p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_MId        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_TyId       p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_BTId       p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_BId        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Flt        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Str        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Int        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_TyVar      p i)) = True
matchParseItem (MP_Item) (PI_MX                      x ) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Wildcard     p)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Nil          p)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_MapMod       p)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Namespace  p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Cmpd       p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Poly       p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Box        p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Lambda   p a b)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Filter     p i)) = True
matchParseItem (MP_Item) (PI_BzSyn (BzS_Curry      p o)) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Box        p x)) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Cmpd       p x)) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Poly       p x)) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Nil        p  )) = True
matchParseItem (MP_Cfg_Line) (PI_Cfg    (LibLine p a b)) = True
matchParseItem (MP_Cfg_Lines)(PI_Cfg    (LibLines  p x)) = True
matchParseItem mp (PI_BzSyn sn) = matchSyntax mp sn
matchParseItem _ _ = False










matchSyntax :: MockParseItem -> BzoSyntax -> Bool
matchSyntax MP_FunDef    (BzS_FunDef    _ _ _ _ _) = True
matchSyntax MP_TypDef    (BzS_TypDef      _ _ _ _) = True
matchSyntax MP_FnTypeDef (BzS_FnTypeDef     _ _ _) = True
matchSyntax MP_Lambda    (BzS_Lambda        _ _ _) = True
matchSyntax MP_Id        (BzS_Id              _ _) = True
matchSyntax MP_TId       (BzS_TyId            _ _) = True
matchSyntax MP_MId       (BzS_MId             _ _) = True
matchSyntax MP_BId       (BzS_BId             _ _) = True
matchSyntax MP_BTId      (BzS_BTId            _ _) = True
matchSyntax MP_Name      (BzS_Namespace       _ _) = True
matchSyntax MP_Int       (BzS_Int             _ _) = True
matchSyntax MP_Flt       (BzS_Flt             _ _) = True
matchSyntax MP_Str       (BzS_Str             _ _) = True
matchSyntax MP_Poly      (BzS_Poly            _ _) = True
matchSyntax MP_Cmpd      (BzS_Cmpd            _ _) = True
matchSyntax MP_Filt      (BzS_Filter          _ _) = True
matchSyntax MP_AGMod     (BzS_ArrGenMod         _) = True
matchSyntax MP_ASMod     (BzS_ArrSzMod        _ _) = True
matchSyntax MP_AXMod     (BzS_ArrExprMod      _ _) = True
matchSyntax MP_FnTy      (BzS_FnTy          _ _ _) = True
matchSyntax MP_Blck      (BzS_Block           _ _) = True
matchSyntax MP_Expr      (BzS_Expr            _ _) = True
matchSyntax MP_Calls     (BzS_Calls           _ _) = True
matchSyntax MP_Wild      (BzS_Wildcard          _) = True
matchSyntax MP_Undef     (BzS_Undefined          ) = True
matchSyntax MP_TyVr      (BzS_TyVar           _ _) = True
matchSyntax MP_ExFunObj  (BzS_ExFunObj      _ _ _) = True
matchSyntax MP_ExTypObj  (BzS_ExTypObj      _ _ _) = True
matchSyntax MP_ArrayObj  (BzS_ArrayObj      _ _ _) = True
matchSyntax MP_FilterObj (BzS_FilterObj     _ _ _) = True
matchSyntax MP_Curry     (BzS_Curry           _ _) = True
matchSyntax MP_CurryObj  (BzS_CurryObj      _ _ _) = True
matchSyntax MP_MapObj    (BzS_MapObj          _ _) = True
matchSyntax MP_MapMod    (BzS_MapMod            _) = True
matchSyntax MP_ArrGnObj  (BzS_ArrGnObj          _) = True
matchSyntax MP_ArrSzObj  (BzS_ArrSzObj        _ _) = True
matchSyntax _ _ = False










-- | Check if two BzoTokens store the same kind of data, not necessarily the
-- | same data exactly
matchBzoToken :: BzoToken -> BzoToken -> Bool
matchBzoToken (TkStartTup  a) (TkStartTup  b) = True
matchBzoToken (TkEndTup    a) (TkEndTup    b) = True
matchBzoToken (TkStartDat  a) (TkStartDat  b) = True
matchBzoToken (TkEndDat    a) (TkEndDat    b) = True
matchBzoToken (TkStartDo   a) (TkStartDo   b) = True
matchBzoToken (TkEndDo     a) (TkEndDo     b) = True
matchBzoToken (TkSepExpr   a) (TkSepExpr   b) = True
matchBzoToken (TkSepPoly   a) (TkSepPoly   b) = True
matchBzoToken (TkFilterSym a) (TkFilterSym b) = True
matchBzoToken (TkLambdaSym a) (TkLambdaSym b) = True
matchBzoToken (TkReference a) (TkReference b) = True
matchBzoToken (TkWildcard  a) (TkWildcard  b) = True
matchBzoToken (TkDefine    a) (TkDefine    b) = True
matchBzoToken (TkFnSym     a) (TkFnSym     b) = True
matchBzoToken (TkTupEmpt   a) (TkTupEmpt   b) = True
matchBzoToken (TkArrGnrl   a) (TkArrGnrl   b) = True
matchBzoToken (TkArrMod    a) (TkArrMod    b) = True
matchBzoToken (TkNewline   a) (TkNewline   b) = True
matchBzoToken (TkCurrySym  a) (TkCurrySym  b) = True
matchBzoToken (TkInt     a b) (TkInt     c d) = True
matchBzoToken (TkFlt     a b) (TkFlt     c d) = True
matchBzoToken (TkStr     a b) (TkStr     c d) = True
matchBzoToken (TkId      a b) (TkId      c d) = True
matchBzoToken (TkTypeId  a b) (TkTypeId  c d) = True
matchBzoToken (TkMutId   a b) (TkMutId   c d) = True
matchBzoToken (TkBuiltin a b) (TkBuiltin c d) = True
matchBzoToken (TkBIType  a b) (TkBIType  c d) = True
matchBzoToken (TkTyVar   a b) (TkTyVar   c d) = True
matchBzoToken (TkNil        ) (TkNil        ) = True
matchBzoToken _               _               = False










genericParseOp :: [MockParseItem] -> ([ParseItem] -> ParseItem) -> ParserOp
genericParseOp mpi xform = ParserOp (\ps ->
  case (match ps mpi) of
    Nothing -> Nothing
    Just (xs, (ParserState f p s i)) -> Just (ParserState f p ([xform $ reverse xs] ++ s) i) )










data ParserState = ParserState{
  fname     :: String,
  lastPos   :: !BzoPos,
  stack     :: ![ParseItem],
  input     :: ![BzoToken] }










data ParserOp = ParserOp{parseop :: ParserState -> Maybe ParserState}










data Parser = Parser{parse :: ParserState -> Either [BzoErr] ParserState}










matchList :: [a] -> [b] -> [a] -> (b -> a -> Bool) -> Maybe ([a], [a])
matchList x [] b cmp = Just (x, b)
matchList x a [] cmp = Nothing
matchList x (a : as) (b : bs) cmp =
  if(cmp a b)
    then matchList (x ++ [b]) as bs cmp
    else Nothing










-- | Specifically for stack
match :: ParserState -> [MockParseItem] -> Maybe ([ParseItem], ParserState)
match (ParserState f p s i) mpi =
  case (matchList [] (reverse mpi) s matchParseItem) of
    Just (s, ss) -> Just (s, (ParserState f p ss i))
    Nothing      -> Nothing










-- | Specifically for lookahead
matchTks :: ParserState -> [BzoToken] -> Maybe ([BzoToken], ParserState)
matchTks (ParserState f p s i) tks =
  case (matchList [] tks i matchBzoToken) of
    Just (i, is) -> Just (i, (ParserState f p s is))
    Nothing      -> Nothing










matchLookahead :: ParserState -> [MockParseItem] -> [BzoToken] -> Maybe ([ParseItem], ParserState)
matchLookahead ps psi bzt =
  let stackOut = match ps psi
      tokenOut = matchTks ps bzt
  in case (stackOut, tokenOut) of
    ((Just (s, (ParserState f p s0 i0))), (Just (i, (ParserState _ _ s1 i1)))) ->
        Just (s, (ParserState f p s0 i1))
    _                                -> Nothing










runParsers :: ParserState -> [Parser] -> Either [BzoErr] ParserState
runParsers pst []       = Left []
runParsers pst (p : ps) = case ((parse p) pst) of
  Left  []   -> runParsers pst ps
  Left  errs -> Left errs
  Right pst' -> Right pst'










tryParsers :: ParserState -> [ParserOp] -> Maybe ParserState
tryParsers pst []       = Nothing
tryParsers pst (p : ps) = case ((parseop p) pst) of
  Nothing   -> tryParsers pst ps
  Just pst' -> Just pst'










shiftParser :: ParserState -> ParserState
shiftParser (ParserState f p s (i : is)) = (ParserState f p ([(PI_Token i)] ++ s) is)
shiftParser (ParserState f p s []) = (ParserState f p s [])










parseIter :: ParserState -> [Parser] -> Either [BzoErr] ParseItem
parseIter ps p =
  case ((runParsers ps p), ps) of
    (_         , (ParserState f p' []  []))                 -> Left $ [ParseErr (BzoPos 0 0 f) "Nothing to Parse?"]
    (_         , (ParserState f p' [PI_SOF]  []))           -> Left $ [ParseErr (BzoPos 0 0 f) "Nothing to Parse?"]
    (Left  []  , (ParserState f p' _   []))                 -> Left $ [ParseErr p' "Parser did not consume entire file."]
    (Left  []  , (ParserState f p' s   i ))                 -> parseIter (shiftParser (ParserState f p' s i)) p
    (Left  errs,                        _)                  -> Left errs              -- | Errors!!
    (Right (ParserState f p' [(PI_BzSyn s)] []),    _)      -> Right (PI_BzSyn s)     -- | Success!!
    (Right (ParserState f p' [(PI_Cfg   s)] []),    _)      -> Right (PI_Cfg   s)     -- | Success!!
    (Right (ParserState f p' s   i ),    _)                 -> parseIter (ParserState f (getPIPos $ head s) s i) p










isBracket :: BzoToken -> Bool
isBracket (TkStartDo  _) = True
isBracket (TkStartTup _) = True
isBracket (TkStartDat _) = True
isBracket (TkEndDo    _) = True
isBracket (TkEndTup   _) = True
isBracket (TkEndDat   _) = True
isBracket _              = False










-- Probably could use a monad, but that's too much work
combineBracketChecks :: ([BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> ([BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
combineBracketChecks f0 f1 tks =
  let a = case (f0 tks) of
            (Just errs, tks') -> (errs, f1 tks')
            (Nothing  , tks') -> ([]  , f1 tks')
  in case a of
      ([],      (Nothing   , tks')) -> (Nothing              , tks')
      (errs0,   (Just errs1, tks')) -> (Just (errs0 ++ errs1), tks')
      (errs0,   (Nothing   , tks')) -> (Just errs0           , tks')











recoverBracketCheck :: ([BzoToken] -> (Maybe [BzoErr], [BzoToken])) -> [BzoErr] -> [BzoToken] -> (Maybe [BzoErr], [BzoToken])
recoverBracketCheck fn errs tks =
  let out = fn tks
  in case out of
    (Nothing   , tks') -> (Just errs           , tks')
    (Just errs0, tks') -> (Just (errs ++ errs0), tks')










bracketCheck_Tuple :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Tuple ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Tuple tks
bracketCheck_Tuple ((TkEndTup   ps) : tks) = (Nothing, tks)
bracketCheck_Tuple ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data  bracketCheck_Tuple tks
bracketCheck_Tuple ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Tuple [ParseErr ps "Invalid placement of ']' inside Tuple"] tks
bracketCheck_Tuple ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Tuple tks
bracketCheck_Tuple ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Tuple [ParseErr ps "Invalid placement of '}' inside Tuple"] tks
bracketCheck_Tuple ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 "Fix Me") "Mismatched parentheses"], [])
bracketCheck_Tuple (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Tuple function"], tks)










bracketCheck_Data :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Data ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Data tks
bracketCheck_Data ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Data [ParseErr ps "Invalid placement of ')' inside Array Modifier"] tks
bracketCheck_Data ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Data  tks
bracketCheck_Data ((TkEndDat   ps) : tks) = (Nothing, tks)
bracketCheck_Data ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Data tks
bracketCheck_Data ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Data [ParseErr ps "Invalid placement of '}' inside Array Modifier"] tks
bracketCheck_Data ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 "Fix Me") "Mismatched Square Brackets"], [])
bracketCheck_Data (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Data function"], tks)










bracketCheck_Block :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Block ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Block tks
bracketCheck_Block ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Block [ParseErr ps "Invalid placement of ')' inside Block"] tks
bracketCheck_Block ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Block  tks
bracketCheck_Block ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Block [ParseErr ps "Invalid placement of ']' inside Block"] tks
bracketCheck_Block ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Block tks
bracketCheck_Block ((TkEndDo    ps) : tks) = (Nothing, tks)
bracketCheck_Block ([]                   ) = (Just $ [ParseErr (BzoPos 0 0 "Fix Me") "Mismatched Braces"], [])
bracketCheck_Block (tk : tks)              = (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Block function"], tks)









bracketCheckFn :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheckFn (t : ts) = case t of
          (TkStartTup ps) -> bracketCheck_Tuple ts
          (TkStartDat ps) -> bracketCheck_Data  ts
          (TkStartDo  ps) -> bracketCheck_Block ts
          (TkEndTup   ps) -> (Just $ [ParseErr ps "Mismatched parentheses!"], ts)
          (TkEndDat   ps) -> (Just $ [ParseErr ps "Mismatched square brackets!"], ts)
          (TkEndDo    ps) -> (Just $ [ParseErr ps "Mismatched braces!"], ts)
          tk              -> (Just $ [ParseErr (spos tk) "This error should not occur. Please notify the developer that something is wrong in the bracketCheckFn function"], ts)










bracketCheck :: [BzoToken] -> Maybe [BzoErr]
bracketCheck tks =
  let tks' = filter isBracket tks
  in checkIter tks' bracketCheckFn
    where checkIter x f = case x of
            [] -> Nothing
            _  -> case (f x) of
                    (Nothing, []) -> Nothing
                    (Nothing, ts) -> checkIter ts f
                    (Just es, []) -> Just es
                    (Just es, ts) -> Just $ maybeMerge es (checkIter ts f)










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
includesASTItem errFn mock sn@(BzS_FilterObj   p x l) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x) ++ (includesASTItem errFn mock l)
includesASTItem errFn mock sn@(BzS_Lambda      p x d) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x) ++ (includesASTItem errFn mock d)
includesASTItem errFn mock sn@(BzS_FnTy        p i o) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock i) ++ (includesASTItem errFn mock o)
includesASTItem errFn mock sn@(BzS_Box           p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_Filter        p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_MapObj        p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_ArrExprMod    p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn@(BzS_Curry         p x) = (incASTHelp0 errFn mock sn) ++ (includesASTItem errFn mock x)
includesASTItem errFn mock sn = (incASTHelp0 errFn mock sn)










fuseNameObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseNameObj (BzS_TyId p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExTypObj p0 i0 i1)]
fuseNameObj (BzS_Id   p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExFunObj p0 i0 i1)]
fuseNameObj a                b                     = [a, b]










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
fuseFilterObj sn@(BzS_TyId  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Id    p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Cmpd  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Poly  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_TyVar p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Box   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_MId   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
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
simplifyASTPass xf f (BzS_FilterObj   p x l) = (BzS_FilterObj  p (passHelp0 xf f x) (passHelp0 xf f l))
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










parseFile :: String -> [BzoToken] -> [Parser] -> Either [BzoErr] BzoSyntax
parseFile f tks ps =
  let bracketErrs = bracketCheck tks
  in case (bracketErrs, parseIter (ParserState f (BzoPos 0 0 f) [PI_SOF] tks) ps) of
      (Just errs,        _ ) -> Left errs
      (Nothing  , Left errs) -> Left errs
      (Nothing  , Right ast) ->
          case (simplifyAST (piSyn ast)) of
            Left errs -> Left errs
            Right out -> Right out
