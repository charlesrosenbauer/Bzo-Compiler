module BzoTypes where










data BzoToken
  = TkStartTup     { spos :: !BzoPos }
  | TkEndTup       { spos :: !BzoPos }
  | TkStartDat     { spos :: !BzoPos }
  | TkEndDat       { spos :: !BzoPos }
  | TkStartDo      { spos :: !BzoPos }
  | TkEndDo        { spos :: !BzoPos }
  | TkSepExpr      { spos :: !BzoPos }
  | TkSepPoly      { spos :: !BzoPos }
  | TkCurrySym     { spos :: !BzoPos }
  | TkFilterSym    { spos :: !BzoPos }
  | TkLambdaSym    { spos :: !BzoPos }
  | TkReference    { spos :: !BzoPos }
  | TkWildcard     { spos :: !BzoPos }
  | TkDefine       { spos :: !BzoPos }
  | TkFnSym        { spos :: !BzoPos }
  | TkTupEmpt      { spos :: !BzoPos }
  | TkArrGnrl      { spos :: !BzoPos }
  | TkArrMod       { spos :: !BzoPos }
  | TkInt          { spos :: !BzoPos, valInt :: !Integer }
  | TkFlt          { spos :: !BzoPos, valFlt :: !Double  }
  | TkStr          { spos :: !BzoPos, valStr :: !String  }
  | TkId           { spos :: !BzoPos, valId  :: !String  }
  | TkTypeId       { spos :: !BzoPos, valId  :: !String  }
  | TkMutId        { spos :: !BzoPos, valId  :: !String  }
  | TkTyVar        { spos :: !BzoPos, valId  :: !String  }
  | TkNewline      { spos :: !BzoPos }
  | TkBuiltin      { spos :: !BzoPos, valId  :: !String  }
  | TkBIType       { spos :: !BzoPos, valId  :: !String  }
  | TkNil
  deriving Eq










data BzoErr = Other
  | StringErr { position::BzoPos, errorStr::String }
  | LexErr    { position::BzoPos, errorStr::String }
  | ParseErr  { position::BzoPos, errorStr::String }
  | TypeErr   { position::BzoPos, errorStr::String }
  | SntxErr   { position::BzoPos, errorStr::String }
  | DepErr    { errorStr::String }
  | ParamErr  { errorStr::String }
  | CfgErr    { errorStr::String }
  | ModelErr  { position::BzoPos, errorStr::String }
  | PrepErr   { position::BzoPos, errorStr::String}










showBzErr :: BzoErr -> String
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ st
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ st
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ st
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ st
showBzErr (SntxErr    p st) = "Syntax Error:\n " ++ (showErrPos p) ++ st
showBzErr (DepErr       st) = "Dependency Error:\n" ++ st
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ st
showBzErr (CfgErr       st) = "Configuration Error:\n" ++ st
showBzErr (PrepErr    p st) = "Preprocessor Error:\n" ++ (showErrPos p) ++ st
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (fileName p) ++ "\", at " ++ (show $ line p) ++ ":" ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: !Int,
  column   :: !Int,
  fileName :: !String }
  deriving (Eq, Show)










showTk :: BzoToken -> String
showTk (TkStartTup      _) = "("
showTk (TkEndTup        _) = ")"
showTk (TkStartDat      _) = "["
showTk (TkEndDat        _) = "]"
showTk (TkStartDo       _) = "{"
showTk (TkEndDo         _) = "}"
showTk (TkSepExpr       _) = "."
showTk (TkSepPoly       _) = ","
showTk (TkCurrySym      _) = "`"
showTk (TkFilterSym     _) = ":"
showTk (TkLambdaSym     _) = ";"
showTk (TkReference     _) = "@"
showTk (TkWildcard      _) = "_"
showTk (TkDefine        _) = "::"
showTk (TkFnSym         _) = ";;"
showTk (TkTupEmpt       _) = "()"
showTk (TkArrGnrl       _) = "[]"
showTk (TkArrMod        _) = ".."
showTk (TkInt        _  x) = "I:"   ++ show x
showTk (TkFlt        _  x) = "F:"   ++ show x
showTk (TkStr        _ st) = "S:"   ++ show st
showTk (TkId         _ st) = "ID:"  ++ show st
showTk (TkMutId      _ st) = "MID:" ++ show st
showTk (TkTypeId     _ st) = "TID:" ++ show st
showTk (TkNewline       _) = "NEWL\n"
showTk (TkBuiltin    _ st) = "BI:"  ++ show st
showTk (TkBIType     _ st) = "BIT:" ++ show st
showTk (TkTyVar      _ st) = "TyVr:" ++ show st
showTk _                   = "NIL"
instance Show BzoToken where show = showTk










showTokens :: [BzoToken] -> String
showTokens tk = unwords $ map showTk tk
