module BzoParser where
import BzoTypes
import BzoSyntax










data ParseItem
  = PI_Token{ piTok :: BzoToken  }
  | PI_BzSyn{ piSyn :: BzoSyntax }










data MockParseItem
  = MP_FunDef
  | MP_TypDef
  | MP_FnTypeDef
  | MP_Lambda
  | MP_Atoms
  | MP_ArrAtm
  | MP_Type
  | MP_Stmt
  | MP_Expr
  | MP_Mods
  | MP_Calls
  | MP_Wild
  | MP_Undef
  | MP_Tk BzoToken










mockPos = BzoPos 0 0 ""










matchParseItem :: MockParseItem -> ParseItem -> Bool
matchParseItem mp (PI_BzSyn sn) = matchSyntax mp sn
matchParseItem (MP_Tk t) (PI_Token tk) = matchBzoToken t tk
matchParseItem _ _ = False










matchSyntax :: MockParseItem -> BzoSyntax -> Bool
matchSyntax MP_FunDef (BzS_FunDef    _ _ _ _ _) = True
matchSyntax MP_TypDef (BzS_TypDef      _ _ _ _) = True
matchSyntax MP_FunDef (BzS_FnTypeDef   _ _ _ _) = True
matchSyntax MP_Lambda (BzS_Lambda        _ _ _) = True
matchSyntax MP_Atoms  (BzS_Atoms           _ _) = True
matchSyntax MP_ArrAtm (BzS_ArrAtom         _ _) = True
matchSyntax MP_Type   (BzS_Type            _ _) = True
matchSyntax MP_Stmt   (BzS_Statements      _ _) = True
matchSyntax MP_Expr   (BzS_Expr            _ _) = True
matchSyntax MP_Mods   (BzS_Modifiers       _ _) = True
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
  let stackOut = match ps (reverse psi)
      tokenOut = matchTks ps bzt
  in case (stackOut, tokenOut) of
    ((Just (s, (ParserState s0 i0))), (Just (i, (ParserState s1 i1)))) ->
        Just (s, (ParserState s0 i1))
    _                                -> Nothing










runParsers :: ParserState -> [Parser] -> Either [BzoErr] ParserState
runParsers pst []       = Left []
runParsers pst (p : ps) = case ((parse p) pst) of
  Left  []   -> runParsers pst ps
  Left  errs -> Left errs
  Right pst' -> Right pst'










tryParsers :: ParserState -> [ParserOp] -> Maybe ParserState
tryParsers pst []       = Just pst
tryParsers pst (p : ps) = case ((parseop p) pst) of
  Nothing   -> tryParsers pst ps
  Just pst' -> Just pst'










shiftParser :: ParserState -> ParserState
shiftParser (ParserState s (i : is)) = (ParserState (s ++ [(PI_Token i)]) is)
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










--parseFile :: [BzoToken] -> Either BzoErr BzoSyntax
