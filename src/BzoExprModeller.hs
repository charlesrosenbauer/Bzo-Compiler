module BzoExprModeller where
import BzoTypes
import BzoEmulator
import HigherOrder
import Data.Text










modelFunctions :: DefinitionTable -> Either [BzoErr] FnTable
modelFunctions (DefinitionTable defs fls ids _) =
  let
      {-
        --Model functions--

        TODO:
          * Convert function expressions to Interpreter Expressions
          * Handle Typeclasses
          * Handle Overloading
          * Handle Scopes
          * Handle Namespaces
      -}

  in Left []










data Var = Var Text BzoPos Int

data VarScope = VarScope [([Var], Int)]



modelExpr :: VarScope -> BzoSyntax -> Either [BzoErr] Expr
modelExpr vs (BzS_Cmpd  _ xs) = toRight (Exp_Cmpd) $ allPass $ Prelude.map (modelExpr vs) xs
modelExpr vs (BzS_Int   _  i) = Right $ Exp_Lit $ Obj_Int $ fromIntegral i
modelExpr vs (BzS_Flt   _  f) = Right $ Exp_Lit $ Obj_Flt f
modelExpr vs (BzS_Str   _  s) = Right $ Exp_Lit $ Obj_Str s
modelExpr vs (BzS_LispCall _ xp os) =
  let
      xp' :: Either [BzoErr] Expr
      xp' = modelExpr vs xp

      os' :: Either [BzoErr] [Expr]
      os' = allPass $ Prelude.map (modelExpr vs) os

      os'':: Either [BzoErr] [Obj]
      os'' = case os' of
              Left ers -> Left ers
              Right xs -> allPass $
                            Prelude.map (\x -> case x of
                                                (Exp_Lit o) -> Right o
                                                x           -> Left [ModelErr (BzoPos 0 0 (pack "<pos lost>")) $ pack "Not a valid value for a prefix expression."]
                                        ) xs



  in case (xp', os'') of
      (Left er0, Left er1) -> Left $ er0 ++ er1
      (Left er0, _       ) -> Left er0
      (_       , Left er1) -> Left er1
      (Right x , Right o ) -> Right $ Exp_Lisp x o

modelExpr vs (BzS_BId   p bid) =
  case (unpack bid) of
    "#add-binop"     -> Right $ Exp_Binop BO_Add
    "#sub-binop"     -> Right $ Exp_Binop BO_Sub
    "#mul-binop"     -> Right $ Exp_Binop BO_Mul
    "#div-binop"     -> Right $ Exp_Binop BO_Div
    "#mod-binop"     -> Right $ Exp_Binop BO_Mod
    "#and-binop"     -> Right $ Exp_Binop BO_And
    "#or-binop"      -> Right $ Exp_Binop BO_Or
    "#xor-binop"     -> Right $ Exp_Binop BO_Xor
    "#rshift-binop"  -> Right $ Exp_Binop BO_Shr
    "#lshift-binop"  -> Right $ Exp_Binop BO_Shl
    "#rrotate-binop" -> Right $ Exp_Binop BO_Rtr
    "#lrotate-binop" -> Right $ Exp_Binop BO_Rtl
    "#popcount-op"   -> Right $ Exp_Unop  UO_Pct
    "#ctlz-op"       -> Right $ Exp_Unop  UO_CLZ
    "#cttz-op"       -> Right $ Exp_Unop  UO_CTZ
    "#neg-op"        -> Right $ Exp_Unop  UO_Neg
    "#not-op"        -> Right $ Exp_Unop  UO_CLZ
    "#trunc"         -> Right $ Exp_Unop  UO_Int
    "#floatcast"     -> Right $ Exp_Unop  UO_Flt
    x                -> Left  $ [ModelErr p $ pack (x ++ " is not a recognized builtin operation.")]
