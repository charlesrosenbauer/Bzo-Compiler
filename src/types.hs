module BzoTypes where










data BzoToken
  = TkStartTup     { spos :: BzoPos }
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
  | TkFlt          { spos :: BzoPos, valFlt :: Double  }
  | TkStr          { spos :: BzoPos, valStr :: String  }
  | TkId           { spos :: BzoPos, valId  :: String  }
  | TkTypeId       { spos :: BzoPos, valId  :: String  }
  | TkMutId        { spos :: BzoPos, valId  :: String  }
  | TkNewline      { spos :: BzoPos }
  | TkBuiltin      { spos :: BzoPos, valId  :: String  }
  | TkBIType       { spos :: BzoPos, valId  :: String  }
  | TkNil
  deriving Eq










data BzoErr = Other
  | StringErr String
  | LexErr String
  | ParseErr String
  | TypeErr String










showBzErr :: BzoErr -> String
showBzErr (StringErr   st) = "Bzo Error: " ++ (show st)
showBzErr (LexErr      st) = "Lexer Error: " ++ (show st)
showBzErr (TypeErr     st) = "Type Error: " ++ (show st)
showBzErr (Other         ) = "Unknown Error?"
instance Show BzoErr where show = showBzErr










data BzoPos = BzoPos {
  line     :: Int,
  column   :: Int,
  fileName :: String }
  deriving (Eq, Show)
