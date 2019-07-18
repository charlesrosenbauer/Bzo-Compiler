module BzoExprModeller where
import BzoTypes
import BzoEmulator
import HigherOrder
import Query
import AST
import Core
import Error
import Data.Text
import Data.List as L









{-
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









{-
  Model Tacit Expressions:
    > Expr with implicit inputs and outputs
    > Convert AST to Expression, which should be pretty straightforward
-}
modelTacitExpr :: [VarScope] -> BzoSyntax -> Either [BzoErr] Expr
modelTacitExpr vs (BzS_Expr _ xs) = Left []









{-
  Model Return Expressions:
    > Expr with implicit output
    > First expression is input
-}
modelRetExpr :: [VarScope] -> BzoSyntax -> Either [BzoErr] Expr
modelRetExpr vs (BzS_Expr _ xs) =
  let
      par = L.head xs

      def = L.tail xs

  in Left []









{-
  Model Standard Expressions:
    > Expr with explicit inputs and outputs
    > First expression is input
    > Last  Expression is output
-}
modelStdExpr :: [VarScope] -> BzoSyntax -> Either [BzoErr] Expr
modelStdExpr vs (BzS_Expr _ xs) =
  let
      par = L.head xs

      ret = L.last xs

      def = L.init $ L.tail xs


  in Left []










data Var = Var Text BzoPos Int

data VarScope = VarScope [([Var], Int)]



modelExpr :: [VarScope] -> BzoSyntax -> Either [BzoErr] Expr
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

      -- TODO: do this in a way that either preserves error locations or prevents errors
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
    "#sine"          -> Right $ Exp_Unop UO_Sin
    "#cosine"        -> Right $ Exp_Unop UO_Cos
    "#tangent"       -> Right $ Exp_Unop UO_Tan
    "#arcsine"       -> Right $ Exp_Unop UO_ASn
    "#arccosine"     -> Right $ Exp_Unop UO_ACs
    "#arctangent"    -> Right $ Exp_Unop UO_ATn
    "#hsine"         -> Right $ Exp_Unop UO_Sinh
    "#hcosine"       -> Right $ Exp_Unop UO_Cosh
    "#htangent"      -> Right $ Exp_Unop UO_Tanh
    "#arc-hsine"     -> Right $ Exp_Unop UO_ASnh
    "#arc-hcosine"   -> Right $ Exp_Unop UO_ACsh
    "#arc-htangent"  -> Right $ Exp_Unop UO_ATnh
    "#log2"          -> Right $ Exp_Unop UO_Lg2
    "#log10"         -> Right $ Exp_Unop UO_Lg10
    "#ln-op"         -> Right $ Exp_Unop UO_Ln
    "#sqrt"          -> Right $ Exp_Unop UO_Sqrt
    "#cbrt"          -> Right $ Exp_Unop UO_Cbrt
    "#exp-unop"      -> Right $ Exp_Unop UO_Exp
    "#exp-binop"     -> Right $ Exp_Binop BO_Pow
    "#surd-binop"    -> Right $ Exp_Binop BO_Srd
    "#log-binop"     -> Right $ Exp_Binop BO_Log
    x                -> Left  $ [ModelErr p $ pack (x ++ " is not a recognized builtin operation.")]

modelExpr vs b@(BzS_Block p xs) =
  let
      varrefs :: [(Text, BzoPos)]
      varrefs = getScopeVars b

      vardefs :: [(Text, BzoPos)]
      vardefs = nubBy  (\(t0,p0)(t1,p1) -> (t0 == t1) && (p0 > p1)) $
                sortBy (\(t0,p0)(t1,p1) -> compare p0 p1) varrefs

      subscps :: [(BzoPos, BzoSyntax)]
      subscps = L.concatMap getSubScopes xs

      scope :: VarScope
      scope = VarScope [] -- Placeholder

      subexps :: [Either [BzoErr] Expr]
      subexps = L.map (modelExpr (scope:vs)) $ L.map snd subscps

  in Left []

-- TODO: handle lambdas and functions using input and output parameters

-- TODO: find some way to handle function calls, type classes, and variables

-- TODO: generate pattern matching structures from parameter lists
-}
