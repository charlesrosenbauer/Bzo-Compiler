module BzoEmulator where
import Data.Text
import Data.Bits










data Obj
  =  Obj_Cmpd [Obj]
  |  Obj_Poly Int Obj
  |  Obj_Int  Int
  |  Obj_Flt  Double
  |  Obj_Str  Text
  |  Obj_Fnc  Int
  |  Obj_Typ  Int [Obj]
  |  Obj_Nil
  |  Obj_Arr  Int Int [Obj]
  |  Obj_Hole
  deriving Show



data Expr
  =  Exp_Binop Binop
  |  Exp_Unop  Unop
  |  Exp_Hof   HOF
  |  Exp_Func
  |  Exp_Map
  |  Exp_Join [Expr]
  |  Exp_Cmpd [Expr]
  |  Exp_Case [(Patn, Expr)]
  |  Exp_Lisp Expr [Obj]
  deriving Show



data Patn
  =  Patn_Cmpd [Patn]
  |  Patn_Poly [Patn]
  |  Patn_Int  Integer
  |  Patn_Flt  Double
  |  Patn_Str  Text
  |  Patn_Fnc  Int
  |  Patn_Typ  Int
  deriving Show




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
  deriving Show



data Unop
  = UO_Not
  | UO_Neg
  | UO_Rev
  | UO_Pct
  | UO_CLZ
  | UO_CTZ
  | UO_Sort
  | UO_Nub
  | UO_Unzip
  deriving Show



data HOF
  = HF_Map
  | HF_Fold
  | HF_Scan
  | HF_Zip
  | HF_Filter
  | HF_SortBy
  | HF_NubBy
  | HF_ZipBy
  deriving Show




apply :: Expr -> Obj -> Obj
apply (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a + b))
apply (Exp_Binop BO_Sub) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a - b))
apply (Exp_Binop BO_Mul) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a * b))
apply (Exp_Binop BO_Div) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `div` b))
apply (Exp_Binop BO_Mod) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `mod` b))

apply (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a + b))
apply (Exp_Binop BO_Sub) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a - b))
apply (Exp_Binop BO_Mul) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a * b))
apply (Exp_Binop BO_Div) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a / b))
--apply (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a `mod` b))

apply (Exp_Binop BO_Shl) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a   b ))
apply (Exp_Binop BO_Shr) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a (-b)))
apply (Exp_Binop BO_Rtl) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a   b ))
apply (Exp_Binop BO_Rtr) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a (-b)))
apply (Exp_Binop BO_Xor) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (xor a b))
apply (Exp_Binop BO_And) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .&. b))
apply (Exp_Binop BO_Or ) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .|. b))

apply (Exp_Unop  UO_Pct) (Obj_Int x) = (Obj_Int (popCount x))
apply (Exp_Unop  UO_CTZ) (Obj_Int x) = (Obj_Int (countTrailingZeros x))
apply (Exp_Unop  UO_CLZ) (Obj_Int x) = (Obj_Int (countLeadingZeros  x))

apply (Exp_Join fs) obj = Prelude.foldr apply obj fs

apply (Exp_Lisp f xs) (Obj_Cmpd ys) =

apply f (Obj_Cmpd [x]) = apply f x
