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
--modelExpr vs (BzS_Int   _  i) = Right $ Obj_Int i
--modelExpr vs (BzS_Flt   _  f) = Right $ Obj_Flt f
--modelExpr vs (BzS_Str   _  s) = Right $ Obj_Str s

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
