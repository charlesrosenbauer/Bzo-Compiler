module BzoChecker where
import BzoSyntax
import BzoTypes
import Data.Map










data BzoTypeErr = BzoTyErr { str :: String, line :: Int }










type IdMap = Map String BzoSyntax










isType :: BzoSyntax -> Bool
isType (TypDef    _ _ _) = True
isType (FnTypeDef _ _ _) = True
isType _                 = False










isFunc :: BzoSyntax -> Bool
isFunc (FunDef _ _ _ _) = True
isFunc _                = False










isExpr :: BzoSyntax -> Bool
isExpr (Expr _) = True
isExpr _        = False










insertType :: IdMap -> BzoSyntax -> IdMap
insertType m (TypDef    p i d) = insert i (TypDef    p i d) m
insertType m (FnTypeDef f i e) = insert i (FnTypeDef f i e) m
insertType m _ = m










insertFunc :: IdMap -> BzoSyntax -> IdMap
insertFunc m (FunDef i f e d) = insert i (FunDef i f e d) m
insertFunc m _ = m










insertExpr :: IdMap -> (Int, BzoSyntax) -> IdMap
insertExpr m i (Expr e) = insert (show i) (Expr e) m
insertExpr m i _ = m










makeIdMaps :: [BzoSyntax] -> Either BzoErr (IdMap, IdMap, IdMap)
makeIdMaps asf = do    -- takes an abstract syntax forest (asf)
  let (typeList, etc0) = break isType asf
  let (funcList, etc1) = break isFunc etc0
  let (exprList, etc2) = break isExpr etc1
  --Unless theres a bug in the parser, etc2 should be empty.
  let typeMap   = Prelude.foldl insertType empty typeList
  let funcMap   = Prelude.foldl insertFunc empty funcList
  let exprList' = zip [1..] (exprList)
  let exprMap   = Prelude.foldl insertExpr empty exprList'
  if (length etc2 > 0)
    then Left  (BzoErr "Something really strange is going on with the parser...\n")
    else Right (typeMap, funcMap, exprMap)












--checkCall :: BzoSyntax -> BzoTypeErr
--checkCall (FunDef    inpr fni expr defn) =
--checkCall (TypDef    prs  tid typ      ) =
--checkCall (FnTypeDef fni  tin tex      ) =
--checkCall (Expr                    expr) =
