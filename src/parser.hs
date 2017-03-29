module BzoParser where
import BzoTypes
import BzoSyntax
import BzoTokens










data ParseItem
  = PI_Token{ piTok :: BzoToken  }
  | PI_BzSyn{ piSyn :: BzoSyntax }
  | PI_Box  { piSyn :: BzoSyntax }
  | PI_CMPD { piSyns:: [BzoSyntax] }
  | PI_POLY { piSyns:: [BzoSyntax] }
  | PI_Fltr { piSyn :: BzoSyntax }
  | PI_LHead{ piPos :: BzoPos, piSyn :: BzoSyntax }
  | PI_Err  { piErr :: String }
  deriving Show










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
  | MP_BId
  | MP_BTId
  | MP_Name
  | MP_Poly
  | MP_Cmpd
  | MP_FnTy
  | MP_Blck
  | MP_Expr
  | MP_Calls
  | MP_Wild
  | MP_Undef
  | MP_Tk BzoToken
  | MP_Tkn
  | MP_Box
  | MP_Cpx
  | MP_Plx
  | MP_Tpx
  | MP_Tup
  | MP_Typ
  | MP_Filt
  | MP_AGMod
  | MP_ASMod
  | MP_AXMod
  | MP_LHead
  | MP_Any










mockPos = BzoPos 0 0 ""
mtk_StartTup  = MP_Tk $ TkStartTup  mockPos
mtk_EndTup    = MP_Tk $ TkEndTup    mockPos
mtk_StartDat  = MP_Tk $ TkStartDat  mockPos
mtk_EndDat    = MP_Tk $ TkEndDat    mockPos
mtk_StartDo   = MP_Tk $ TkStartDo   mockPos
mtk_EndDo     = MP_Tk $ TkEndDo     mockPos
mtk_SepExpr   = MP_Tk $ TkSepExpr   mockPos
mtk_SepPoly   = MP_Tk $ TkSepPoly   mockPos
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
mtk_Nil       = MP_Tk $ TkNil











matchParseItem :: MockParseItem -> ParseItem -> Bool
matchParseItem (MP_Any ) _                         = True
matchParseItem (MP_LHead)(PI_LHead           _ _ ) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Cmpd p x)) = True
matchParseItem (MP_Tup ) (PI_BzSyn (BzS_Poly p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Cmpd p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_Poly p x)) = True
matchParseItem (MP_Typ ) (PI_BzSyn (BzS_TyId p x)) = True
matchParseItem (MP_Typ ) (PI_Box   (BzS_TyId p x)) = True
matchParseItem mp (PI_BzSyn sn) = matchSyntax mp sn
matchParseItem (MP_Tk t) (PI_Token tk) = matchBzoToken t tk
matchParseItem (MP_Tkn ) (PI_Token tk) = True
matchParseItem (MP_Box ) (PI_Box   sn) = True
matchParseItem (MP_Cpx ) (PI_CMPD  xs) = True
matchParseItem (MP_Plx ) (PI_POLY  xs) = True
matchParseItem (MP_Tpx ) (PI_POLY  xs) = True
matchParseItem (MP_Tpx ) (PI_CMPD  xs) = True
matchParseItem _ _ = False










matchSyntax :: MockParseItem -> BzoSyntax -> Bool
matchSyntax MP_FunDef (BzS_FunDef    _ _ _ _ _) = True
matchSyntax MP_TypDef (BzS_TypDef      _ _ _ _) = True
matchSyntax MP_FunDef (BzS_FnTypeDef   _ _ _ _) = True
matchSyntax MP_Lambda (BzS_Lambda        _ _ _) = True
matchSyntax MP_Id     (BzS_Id              _ _) = True
matchSyntax MP_TId    (BzS_TyId            _ _) = True
matchSyntax MP_MId    (BzS_MId             _ _) = True
matchSyntax MP_BId    (BzS_BId             _ _) = True
matchSyntax MP_BTId   (BzS_BTId            _ _) = True
matchSyntax MP_Name   (BzS_Namespace       _ _) = True
matchSyntax MP_Int    (BzS_Int             _ _) = True
matchSyntax MP_Flt    (BzS_Flt             _ _) = True
matchSyntax MP_Str    (BzS_Str             _ _) = True
matchSyntax MP_Poly   (BzS_Poly            _ _) = True
matchSyntax MP_Cmpd   (BzS_Cmpd            _ _) = True
matchSyntax MP_Filt   (BzS_Filter          _ _) = True
matchSyntax MP_AGMod  (BzS_ArrGenMod         _) = True
matchSyntax MP_ASMod  (BzS_ArrSzMod        _ _) = True
matchSyntax MP_AXMod  (BzS_ArrExprMod      _ _) = True
matchSyntax MP_FnTy   (BzS_FnTy          _ _ _) = True
matchSyntax MP_Blck   (BzS_Block           _ _) = True
matchSyntax MP_Expr   (BzS_Expr            _ _) = True
matchSyntax MP_Calls  (BzS_Calls           _ _) = True
matchSyntax MP_Wild   (BzS_Wildcard          _) = True
matchSyntax MP_Undef  (BzS_Undefined         _) = True
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
matchBzoToken (TkInt     a b) (TkInt     c d) = True
matchBzoToken (TkFlt     a b) (TkFlt     c d) = True
matchBzoToken (TkStr     a b) (TkStr     c d) = True
matchBzoToken (TkId      a b) (TkId      c d) = True
matchBzoToken (TkTypeId  a b) (TkTypeId  c d) = True
matchBzoToken (TkMutId   a b) (TkMutId   c d) = True
matchBzoToken (TkBuiltin a b) (TkBuiltin c d) = True
matchBzoToken (TkBIType  a b) (TkBIType  c d) = True
matchBzoToken (TkNil        ) (TkNil        ) = True
matchBzoToken _               _               = False










data ParserState = ParserState{
  stack     :: [ParseItem],
  input     :: [BzoToken] }










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
match (ParserState s i) mpi =
  case (matchList [] (reverse mpi) s matchParseItem) of
    Just (s, ss) -> Just (s, (ParserState ss i))
    Nothing      -> Nothing










-- | Specifically for lookahead
matchTks :: ParserState -> [BzoToken] -> Maybe ([BzoToken], ParserState)
matchTks (ParserState s i) tks =
  case (matchList [] tks i matchBzoToken) of
    Just (i, is) -> Just (i, (ParserState s is))
    Nothing      -> Nothing










matchLookahead :: ParserState -> [MockParseItem] -> [BzoToken] -> Maybe ([ParseItem], ParserState)
matchLookahead ps psi bzt =
  let stackOut = match ps psi
      tokenOut = matchTks ps bzt
  in case (stackOut, tokenOut) of
    ((Just (s, (ParserState s0 i0))), (Just (i, (ParserState s1 i1)))) ->
        Just (s, (ParserState s0 i1))
    _                                -> Nothing










matchNotLookahead :: ParserState -> [MockParseItem] -> [BzoToken] -> Maybe ([ParseItem], ParserState)
matchNotLookahead ps psi bzt =
  let stackOut = match ps psi
      tokenOut = matchTks ps bzt
  in case (stackOut, tokenOut) of
    ((Just (s, (ParserState s0 i0))), Nothing) ->
        Just (s, (ParserState s0 i0))
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
shiftParser (ParserState s (i : is)) = (ParserState ([(PI_Token i)] ++ s) is)
shiftParser (ParserState s []) = (ParserState s [])










parseIter :: ParserState -> [Parser] -> Either [BzoErr] BzoSyntax
parseIter ps p =
  case ((runParsers ps p), ps) of
    (_         , (ParserState []  []))                 -> Left $ [ParseErr "Nothing to Parse?"]
    (Left  []  , (ParserState _   []))                 -> Left $ [ParseErr "Parser did not consume entire file."]
    (Left  []  , (ParserState s   i ))                 -> parseIter (shiftParser (ParserState s i)) p
    (Left  errs,                    _)                 -> Left errs        -- | Errors!!
    (Right (ParserState [(PI_BzSyn s)] []),    _)      -> Right s          -- | Success!!
    (Right (ParserState s   i ),    _)                 -> parseIter (ParserState s i) p










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
bracketCheck_Tuple ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Tuple [ParseErr "Invalid placement of ']' inside Tuple"] tks
bracketCheck_Tuple ((TkStartDo  ps) : tks) = recoverBracketCheck bracketCheck_Tuple [ParseErr "Invalid placement of '{' inside Tuple"] tks
bracketCheck_Tuple ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Tuple [ParseErr "Invalid placement of '}' inside Tuple"] tks
bracketCheck_Tuple ([]                   ) = (Just $ [ParseErr "Mismatched parentheses"], [])
bracketCheck_Tuple (_ : tks)               = (Just $ [ParseErr "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Tuple function"], tks)










bracketCheck_Data :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Data ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Data tks
bracketCheck_Data ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Data [ParseErr "Invalid placement of ')' inside Array Modifier"] tks
bracketCheck_Data ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Data  tks
bracketCheck_Data ((TkEndDat   ps) : tks) = (Nothing, tks)
bracketCheck_Data ((TkStartDo  ps) : tks) = recoverBracketCheck bracketCheck_Data [ParseErr "Invalid placement of '{' inside Array Modifier"] tks
bracketCheck_Data ((TkEndDo    ps) : tks) = recoverBracketCheck bracketCheck_Data [ParseErr "Invalid placement of '}' inside Array Modifier"] tks
bracketCheck_Data ([]                   ) = (Just $ [ParseErr "Mismatched Square Brackets"], [])
bracketCheck_Data (_ : tks)               = (Just $ [ParseErr "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Data function"], tks)










bracketCheck_Block :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheck_Block ((TkStartTup ps) : tks) = combineBracketChecks bracketCheck_Tuple bracketCheck_Block tks
bracketCheck_Block ((TkEndTup   ps) : tks) = recoverBracketCheck bracketCheck_Block [ParseErr "Invalid placement of ')' inside Block"] tks
bracketCheck_Block ((TkStartDat ps) : tks) = combineBracketChecks bracketCheck_Data bracketCheck_Block  tks
bracketCheck_Block ((TkEndDat   ps) : tks) = recoverBracketCheck bracketCheck_Block [ParseErr "Invalid placement of ']' inside Block"] tks
bracketCheck_Block ((TkStartDo  ps) : tks) = combineBracketChecks bracketCheck_Block bracketCheck_Block tks
bracketCheck_Block ((TkEndDo    ps) : tks) = (Nothing, tks)
bracketCheck_Block ([]                   ) = (Just $ [ParseErr "Mismatched Braces"], [])
bracketCheck_Block (_ : tks)               = (Just $ [ParseErr "This error should not occur. Please notify the developer that something is wrong in the bracketCheck_Block function"], tks)









bracketCheckFn :: [BzoToken] -> (Maybe [BzoErr], [BzoToken])
bracketCheckFn (t : ts) = case t of
          (TkStartTup ps) -> bracketCheck_Tuple ts
          (TkStartDat ps) -> bracketCheck_Data  ts
          (TkStartDo  ps) -> bracketCheck_Block ts
          (TkEndTup   ps) -> (Just $ [ParseErr "Mismatched parentheses!"], ts)
          (TkEndDat   ps) -> (Just $ [ParseErr "Mismatched square brackets!"], ts)
          (TkEndDo    ps) -> (Just $ [ParseErr "Mismatched braces!"], ts)
          _               -> (Just $ [ParseErr "This error should not occur. Please notify the developer that something is wrong in the bracketCheckFn function"], ts)










maybeMerge :: [a] -> Maybe [a] -> [a]
maybeMerge a (Just b) = a ++ b
maybeMerge a _        = a










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









parseFile :: [BzoToken] -> [Parser] -> Either [BzoErr] BzoSyntax
parseFile tks ps =
  let bracketErrs = bracketCheck tks
  in case (bracketErrs, parseIter (ParserState [] tks) ps) of
      (Just errs,        _ ) -> Left errs
      (Nothing  , Left errs) -> Left errs
      (Nothing  , Right ast) -> Right ast
