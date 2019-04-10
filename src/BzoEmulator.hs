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
  =  Exp_Binop
  |  Exp_Unop
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
