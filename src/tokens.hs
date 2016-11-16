module BzoTokens where
import BzoTypes











showTk :: BzoToken -> String
showTk (TkStartTup      _) = "("
showTk (TkEndTup        _) = ")"
showTk (TkStartDat      _) = "["
showTk (TkEndDat        _) = "]"
showTk (TkStartDo       _) = "{"
showTk (TkEndDo         _) = "}"
showTk (TkSepExpr       _) = "."
showTk (TkSepPoly       _) = ","
showTk (TkFilterSym     _) = ":"
showTk (TkLambdaSym     _) = ";"
showTk (TkMutable       _) = "~"
showTk (TkReference     _) = "@"
showTk (TkWildcard      _) = "_"
showTk (TkDefine        _) = "::"
showTk (TkFnSym         _) = ";;"
showTk (TkTupEmpt       _) = "()"
showTk (TkArrGnrl       _) = "[]"
showTk (TkExpGnrl       _) = "{}"
showTk (TkArrMod        _) = ".."
showTk (TkInt        _  x) = "I:"   ++ show x
showTk (TkFlt        _  x) = "F:"   ++ show x
showTk (TkStr        _ st) = "S:"   ++ show st
showTk (TkId         _ st) = "ID:"  ++ show st
showTk (TkTypeId     _ st) = "TID:" ++ show st
showTk (TkVariable _ st _) = "VR:"  ++ show st
showTk (TkFunction _ st _) = "FN:"  ++ show st
showTk (TkTypeVar  _ st  ) = "TV:"  ++ show st
showTk (TkLambda        _) = "LMDA"
showTk (TkExpr          _) = "EXPR"
showTk (TkNewline        ) = "NEWL\n"
showTk (TkBuiltin    _ st) = "BI:"  ++ show st
showTk _                   = "Unknown Token?"
instance Show BzoToken where show = showTk










showTokens :: [BzoToken] -> String
showTokens tk = unwords $ map showTk tk
