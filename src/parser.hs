module BzoParser where
import BzoTypes










data BzoSyntax
  = BzSCalls {
    synPos   :: BzoPos,
    synCalls :: [BzoSyntax] }
  | BzSFnDef {
    synPos   :: BzoPos,
    synIPars :: BzoSyntax,
    synEPars :: BzoSyntax,
    synFnid  :: String,
    synFndef :: BzoSyntax }
  | BzSFTDef {
    synPos   :: BzoPos,
    synIPars :: BzoSyntax,
    synEPars :: BzoSyntax,
    synFnid  :: String }
  | BzSTyDef {
    synPos   :: BzoPos,
    synIPars :: BzoSyntax,
    synTid   :: String,
    synTDef  :: BzoSyntax }
  | BzSExprAtom {
    synPos   :: BzoPos,
    synId    :: String }
  | BzSExpr {
    synPos   :: BzoPos,
    synExpr  :: [BzoSyntax] }
  | BzSNil










mockBzoPos      = (BzoPos 0 0 "")
mockBzSCalls    = (BzSCalls    mockBzoPos [])
mockBzSFnDef    = (BzSFnDef    mockBzoPos BzSNil BzSNil "" BzSNil)
mockBzSFTDef    = (BzSFTDef    mockBzoPos BzSNil BzSNil "")
mockBzSTyDef    = (BzSTyDef    mockBzoPos BzSNil "" BzSNil)
mockBzSExprAtom = (BzSExprAtom mockBzoPos "")
mockBzSExpr     = (BzSExpr     mockBzoPos [])








data ParseItem
  = PI_Token{ piTok :: BzoToken  }
  | PI_BzSyn{ piSyn :: BzoSyntax }









data ParserState = ParserState{
  stack     :: [ParseItem],
  input     :: [BzoToken] }










data Parser = Parser{ parse :: ParserState -> Maybe ParserState }










matchList :: [a] -> [a] -> [a] -> (a -> a -> Bool) -> Maybe ([a], [a])
matchList x [] b cmp = Just (x, b)
matchList x a [] cmp = Nothing
matchList x (a : as) (b : bs) cmp =
  if(cmp a b)
    then matchList (x ++ [b]) as bs cmp
    else Nothing










matchBzoSyntax :: BzoSyntax -> BzoSyntax -> Bool
matchBzoSyntax (BzSCalls          a0 b0) (BzSCalls          a1 b1) = True
matchBzoSyntax (BzSFnDef a0 b0 c0 d0 e0) (BzSFnDef a1 b1 c1 d1 e1) = True
matchBzoSyntax (BzSFTDef    a0 b0 c0 d0) (BzSFTDef    a1 b1 c1 d1) = True
matchBzoSyntax (BzSTyDef    a0 b0 c0 d0) (BzSTyDef    a1 b1 c1 d1) = True
matchBzoSyntax (BzSExprAtom       a0 b0) (BzSExprAtom       a1 b1) = True
matchBzoSyntax (BzSExpr           a0 b0) (BzSExpr           a1 b1) = True
matchBzoSyntax (BzSNil                 ) (BzSNil                 ) = True
matchBzoSyntax _                         _                         = False










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
matchBzoToken (TkMutable   a) (TkMutable   b) = True
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










matchParseItem :: ParseItem -> ParseItem -> Bool
matchParseItem (PI_Token a) (PI_Token b) = matchBzoToken  a b
matchParseItem (PI_BzSyn a) (PI_BzSyn b) = matchBzoSyntax a b
matchParseItem _            _            = False











matchStack :: ParserState -> [ParseItem] -> Maybe ([ParseItem], [ParseItem])
matchStack (ParserState s i) psi = matchList [] s psi matchParseItem











matchTokens :: ParserState -> [BzoToken] -> Maybe ([BzoToken], [BzoToken])
matchTokens (ParserState s i) bzt = matchList [] i bzt matchBzoToken










matchPattern :: ParserState -> [ParseItem] -> [BzoToken] -> Maybe ([ParseItem], ParserState)
matchPattern (ParserState s i) psi bzt =
  let stackOut = matchStack  (ParserState s i) psi
      tokenOut = matchTokens (ParserState s i) bzt
  in case (stackOut, tokenOut) of
    ((Just (s, ss)), (Just (i, is))) -> Just (s, (ParserState ss (i ++ is)))
    _                                -> Nothing










shiftParser :: ParserState -> ParserState
shiftParser (ParserState s (i : is)) = (ParserState (s ++ [(PI_Token i)]) is)
shiftParser (ParserState s []) = (ParserState s [])










tryParsers :: ParserState -> [Parser] -> Maybe ParserState
tryParsers s       [] = Nothing
tryParsers s (p : ps) = case ((parse p) s) of
  Just x  -> Just x
  Nothing -> tryParsers s ps










parseCalls_0 :: Parser
parseCalls_0 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSCalls), (PI_BzSyn mockBzSCalls)] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSCalls p0 c0)), (PI_BzSyn (BzSCalls p1 c1))], (ParserState s i)) ->
      Just (ParserState ( s ++ [(PI_BzSyn (BzSCalls p0 (c0 ++ c1)))]) i)
    Just _  -> Nothing    )










parseCalls_1 :: Parser
parseCalls_1 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSFnDef)] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSFnDef p0 a b c d))], (ParserState s i)) ->
      Just (ParserState (s ++ [(PI_BzSyn (BzSCalls p0 [(BzSFnDef p0 a b c d)]))]) i)
    Just _  -> Nothing  )










parseCalls_2 :: Parser
parseCalls_2 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSFTDef)] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSFTDef p0 a b c))], (ParserState s i)) ->
      Just (ParserState (s ++ [(PI_BzSyn (BzSCalls p0 [(BzSFTDef p0 a b c)]))]) i)
    Just _  -> Nothing    )










parseCalls_3 :: Parser
parseCalls_3 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSTyDef)] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSTyDef p0 a b c))], (ParserState s i)) ->
      Just (ParserState (s ++ [(PI_BzSyn (BzSCalls p0 [(BzSTyDef p0 a b c)]))]) i)
    Just _  -> Nothing    )










parseCalls_4 :: Parser
parseCalls_4 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSExpr)] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSExpr p0 a))], (ParserState s i)) ->
      Just (ParserState (s ++ [(PI_BzSyn (BzSCalls p0 [(BzSExpr p0 a)]))]) i)
    Just _  -> Nothing    )










parseCalls_5 :: Parser
parseCalls_5 = Parser (\ps ->
  case matchPattern ps [(PI_BzSyn mockBzSCalls), (PI_Token (TkNewline mockBzoPos))] [] of
    Nothing -> Nothing
    Just ([(PI_BzSyn (BzSCalls p0 a)), (PI_Token (TkNewline mockBzoPos))], (ParserState s i)) ->
      Just (ParserState (s ++ [(PI_BzSyn (BzSCalls p0 a))]) i)
    Just _  -> Nothing    )










parseCalls :: [Parser]
parseCalls = [parseCalls_0, parseCalls_1, parseCalls_2,
              parseCalls_3, parseCalls_4, parseCalls_5]










-- | Inputs / Outputs:
-- |    Token Inputs
-- |    isREPL
-- |
--parseFile :: [BzoToken] -> Bool -> BzoSyntax
