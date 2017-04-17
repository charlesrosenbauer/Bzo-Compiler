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
  | StringErr { position::BzoPos, errorStr::String }
  | LexErr    { position::BzoPos, errorStr::String }
  | ParseErr  { position::BzoPos, errorStr::String }
  | TypeErr   { position::BzoPos, errorStr::String }
  | ParamErr  { errStr::String }
  | PrepErr   { errStr::String}










showBzErr :: BzoErr -> String
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ (show st)
showBzErr (PrepErr      st) = "Preprocessor Error:\n" ++ (show st)
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (fileName p) ++ "\", at " ++ (show $ line p) ++ ":" ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: Int,
  column   :: Int,
  fileName :: String }
  deriving (Eq, Show)
