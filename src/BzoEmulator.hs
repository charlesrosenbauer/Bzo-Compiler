module BzoEmulator where
import Data.Text










data Obj
  =  Obj_Cmpd [Obj]
  |  Obj_Poly Int Obj
  |  Obj_Int  Integer
  |  Obj_Flt  Double
  |  Obj_Str  Text
  |  Obj_Fnc  Int
  |  Obj_Typ  Int [Obj]
  |  Obj_Nil
  |  Obj_Arr  Int Int [Obj]



data Expr
  =  Exp_Binop Binop
  |  Exp_Unop  Unop
  |  Exp_Hof   HOF
  |  Exp_Func
  |  Exp_Map
  |  Exp_Join [Expr]
  |  Exp_Cmpd [Expr]
  |  Exp_Case [(Patn, Expr)]



data Patn
  =  Patn_Cmpd [Patn]
  |  Patn_Poly [Patn]
  |  Patn_Int  Integer
  |  Patn_Flt  Double
  |  Patn_Str  Text
  |  Patn_Fnc  Int
  |  Patn_Typ  Int




data Binop
  = BO_Add
  | BO_Sub
  | BO_Mul
  | BO_Div
  | BO_Mod
  | BO_Xor
  | BO_And
  | BO_Or
  | BO_Rtr
  | BO_Rtl
  | BO_Shr
  | BO_Shl
  | BO_Drop
  | BO_Take
  | BO_Zip



data Unop
  = UO_Not
  | UO_Neg
  | UO_Rev
  | UO_Sort
  | UO_Nub
  | UO_Unzip



data HOF
  = HF_Map
  | HF_Fold
  | HF_Scan
  | HF_Zip
  | HF_Filter
  | HF_SortBy
  | HF_NubBy
  | HF_ZipBy




apply :: Expr -> Obj -> Obj
apply (Expr_Binop BO_Add) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a + b))
apply (Expr_Binop BO_Sub) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a - b))
apply (Expr_Binop BO_Mul) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a * b))
apply (Expr_Binop BO_Div) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `div` b))
apply (Expr_Binop BO_Add) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `mod` b))

apply (Expr_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a + b))
apply (Expr_Binop BO_Sub) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a - b))
apply (Expr_Binop BO_Mul) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a * b))
apply (Expr_Binop BO_Div) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a / b))
--apply (Expr_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a `mod` b))
