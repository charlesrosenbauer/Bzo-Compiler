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
    synFndef :: BzoPos }
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










data ParseItem
  = PI_Token{ piTok :: BzoToken  }
  | PI_BzSyn{ piSyn :: BzoSyntax }









data ParserState = ParserState{
  stack     :: [ParseItem],
  input     :: [BzoToken] }










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











-- | Inputs / Outputs:
-- |    Token Inputs
-- |    isREPL
-- |
--parseFile :: [BzoToken] -> Bool -> BzoSyntax
