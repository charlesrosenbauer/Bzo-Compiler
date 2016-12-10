module BzoTypes where










data BzoToken = TkStartTup  { spos :: BzoPos }
  | TkEndTup       { spos :: BzoPos }
  | TkStartDat     { spos :: BzoPos }
  | TkEndDat       { spos :: BzoPos }
  | TkStartDo      { spos :: BzoPos }
  | TkEndDo        { spos :: BzoPos }
  | TkSepExpr      { spos :: BzoPos }
  | TkSepPoly      { spos :: BzoPos }
  | TkFilterSym    { spos :: BzoPos }
  | TkLambdaSym    { spos :: BzoPos }
  | TkMutable      { spos :: BzoPos }
  | TkReference    { spos :: BzoPos }
  | TkWildcard     { spos :: BzoPos }
  | TkDefine       { spos :: BzoPos }
  | TkFnSym        { spos :: BzoPos }
  | TkTupEmpt      { spos :: BzoPos }
  | TkArrGnrl      { spos :: BzoPos }
  | TkArrMod       { spos :: BzoPos }
  | TkInt          { spos :: BzoPos, valInt :: Integer }
  | TkFlt          { spos :: BzoPos, valFlt :: Float   }
  | TkStr          { spos :: BzoPos, valStr :: String  }
  | TkId           { spos :: BzoPos, valId  :: String  }
  | TkTypeId       { spos :: BzoPos, valId  :: String  }
  | TkNewline      { spos :: BzoPos }
  | TkBuiltin      { spos :: BzoPos, valId  :: String  }
  | TkBIType       { spos :: BzoPos, valId  :: String  }
  deriving Eq










data BzoErr = Other
  | StringErr String
  | LexErr String










-- Creating a custom position type so that Parsec types don't have to be moved around everywhere.
-- This also makes it easy to add extra information here later to pass around
data BzoPos = BzoPos {
  line     :: Int,
  column   :: Int,
  fileName :: String }
  deriving (Eq, Show)










countLines :: [BzoToken] -> Int -> [Int]
countLines ((TkNewline t):ts) n = [n + 1] ++ (countLines ts (n + 1))
countLines (t:ts) n = [n] ++ (countLines ts n)










getLineNumbers :: [BzoToken] -> [BzoToken]
getLineNumbers tk = do
  let linenums = countLines tk 0
  map (\(t, n) -> (tkChangePos t n)) $ zip tk linenums










--tkChangePos :: BzoToken -> Int -> BzoToken
tkChangePos (TkStartTup    p) n = TkStartTup    $ BzoPos n (column p) (fileName p)
tkChangePos (TkEndTup      p) n = TkEndTup      $ BzoPos n (column p) (fileName p)
tkChangePos (TkStartDat    p) n = TkStartDat    $ BzoPos n (column p) (fileName p)
tkChangePos (TkEndDat      p) n = TkEndDat      $ BzoPos n (column p) (fileName p)
tkChangePos (TkStartDo     p) n = TkStartDo     $ BzoPos n (column p) (fileName p)
tkChangePos (TkEndDo       p) n = TkEndDo       $ BzoPos n (column p) (fileName p)
tkChangePos (TkSepExpr     p) n = TkSepExpr     $ BzoPos n (column p) (fileName p)
tkChangePos (TkSepPoly     p) n = TkSepPoly     $ BzoPos n (column p) (fileName p)
tkChangePos (TkFilterSym   p) n = TkFilterSym   $ BzoPos n (column p) (fileName p)
tkChangePos (TkLambdaSym   p) n = TkLambdaSym   $ BzoPos n (column p) (fileName p)
tkChangePos (TkMutable     p) n = TkMutable     $ BzoPos n (column p) (fileName p)
tkChangePos (TkReference   p) n = TkReference   $ BzoPos n (column p) (fileName p)
tkChangePos (TkWildcard    p) n = TkWildcard    $ BzoPos n (column p) (fileName p)
tkChangePos (TkDefine      p) n = TkDefine      $ BzoPos n (column p) (fileName p)
tkChangePos (TkFnSym       p) n = TkFnSym       $ BzoPos n (column p) (fileName p)
tkChangePos (TkTupEmpt     p) n = TkTupEmpt     $ BzoPos n (column p) (fileName p)
tkChangePos (TkArrGnrl     p) n = TkArrGnrl     $ BzoPos n (column p) (fileName p)
tkChangePos (TkArrMod      p) n = TkArrMod      $ BzoPos n (column p) (fileName p)
tkChangePos (TkInt       p i) n = TkInt         ( BzoPos n (column p) (fileName p)) i
tkChangePos (TkFlt       p f) n = TkFlt         ( BzoPos n (column p) (fileName p)) f
tkChangePos (TkStr       p s) n = TkStr         ( BzoPos n (column p) (fileName p)) s
tkChangePos (TkId        p i) n = TkId          ( BzoPos n (column p) (fileName p)) i
tkChangePos (TkTypeId    p i) n = TkTypeId      ( BzoPos n (column p) (fileName p)) i
tkChangePos (TkNewline     p) n = TkNewline     ( BzoPos n (column p) (fileName p))
tkChangePos (TkBuiltin   p i) n = TkBuiltin     ( BzoPos n (column p) (fileName p)) i
tkChangePos (TkBIType    p i) n = TkBIType      ( BzoPos n (column p) (fileName p)) i
