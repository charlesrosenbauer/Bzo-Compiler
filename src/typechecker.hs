module BzoChecker where
import BzoSyntax










data BzoTypeErr = BzoTyErr { str :: String, line :: Int }










checkCall :: BzoSyntax -> BzoTypeErr
--checkCall (FunDef    inpr fni expr defn) =
--checkCall (TypDef    prs  tid typ      ) =
--checkCall (FnTypeDef fni  tin tex      ) =
--checkCall (Expr                    expr) = 
