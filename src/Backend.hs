module Backend where
import System.IO
import BackendTypes
import BzoTypes










printGoHeader :: [String] -> String
printGoHeader imports = "package main\n\nimport (\n" ++ (concatMap (\x -> "\"" ++ x ++"\"\n") imports) ++ ")\n\n"







printFunction :: String -> [String] -> [String] -> String -> String
printFunction fname inpars expars contents =
  "\nfunc " ++ fname ++ "(" ++ (concatMap (++",") inpars) ++ ")" ++
                        "(" ++ (concatMap (++",") expars) ++ "){\n" ++
                        contents ++ "\n}\n"



printBinop :: BGArithop -> String
printBinop BGIADD = "+"
printBinop BGISUB = "-"
printBinop BGIMUL = "*"
printBinop BGIDIV = "/"
printBinop BGIMOD = "%"
printBinop BGFADD = "+"
printBinop BGFSUB = "-"
printBinop BGFMUL = "*"
printBinop BGFDIV = "/"
printBinop BGBOR  = "|"
printBinop BGBAND = "&"
printBinop BGBXOR = "^"
printBinop BGLOR  = "||"
printBinop BGLAND = "&&"
printBinop BGLSS  = "<"
printBinop BGGTR  = ">"
printBinop BGLSE  = "<="
printBinop BGGTE  = ">="
printBinop BGNEQ  = "!="
printBinop BGEQ   = "=="
printBinop BGBSHL = "<<"
printBinop BGBSHR = ">>"




printId :: BGId -> String
printId 0 = "_"
printId x = " X" ++ (show x) ++ " "




printExpr :: BGExpr -> String
printExpr (BGArith BGBNOT x _) = " (^" ++ (printExpr x) ++ ") "
printExpr (BGArith BGLNOT x _) = " (!" ++ (printExpr x) ++ ") "
printExpr (BGArith BGEZ   x _) = " (" ++ (printExpr x) ++ " == 0) "
printExpr (BGArith BGNZ   x _) = " (" ++ (printExpr x) ++ " != 0) "
printExpr (BGArith op a b    ) = " ("  ++ (printExpr a) ++ (printBinop op) ++ (printExpr b) ++ ") "
printExpr (BGVar    x)         = printId x
printExpr (BGVrSet    vs expr) = (concatMap (\x -> (printId x) ++ ", ") vs) ++ " := " ++ (printExpr expr)
