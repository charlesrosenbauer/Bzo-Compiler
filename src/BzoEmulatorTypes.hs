module BzoEmulatorTypes where
import Data.Text
import Data.Bits
import Data.Map.Strict
import Data.Set










data Obj
  =  Obj_Cmpd [Obj]
  |  Obj_Poly Int Obj   -- This is basically a tagged union
  |  Obj_Int  Int
  |  Obj_Flt  Double
  |  Obj_Str  Text
  |  Obj_Fnc  Int
  |  Obj_Var  Int
  |  Obj_Bl   Bool
  |  Obj_Typ  Int [Obj]  -- This includes a type tag
  |  Obj_Nil
  |  Obj_Arr  Int [Obj]
  |  Obj_Hole
  |  Obj_Fault Text
  |  Obj_Expr Expr
  deriving Show



data Expr
  =  Exp_Binop Binop
  |  Exp_Unop  Unop
  |  Exp_Lit   Obj
  |  Exp_Hof   HOF
  |  Exp_Func  Int
  |  Exp_Map
  |  Exp_Join [Expr]
  |  Exp_Cmpd [Expr]
  |  Exp_Case [(Patn, Expr)]
  |  Exp_Lisp Expr [Obj]
  |  Exp_RVar  Int
  |  Exp_WVar  Int
  deriving Show



data Patn
  =  Patn_Cmpd [Patn]
  |  Patn_Poly [Patn]
  |  Patn_Int  Integer
  |  Patn_Flt  Double
  |  Patn_Str  Text
  |  Patn_Bl   Bool
  |  Patn_Vr   Int
  |  Patn_Nil
  |  Patn_Fnc  Int
  |  Patn_Typ  Int [Patn]
  |  Patn_TVar Int [Patn]
  |  Patn_Wild
  deriving Show




data Binop
  = BO_Add
  | BO_Sub
  | BO_Mul
  | BO_Div
  | BO_Mod
  | BO_Eq
  | BO_NEq
  | BO_Ls
  | BO_Gt
  | BO_Lse
  | BO_Gte
  | BO_Xor
  | BO_And
  | BO_Or
  | BO_Rtr
  | BO_Rtl
  | BO_Shr
  | BO_Shl
  | BO_Log
  | BO_Pow
  | BO_Srd
  | BO_Drop
  | BO_Take
  | BO_Zip
  deriving Show



data Unop
  = UO_Not
  | UO_E0
  | UO_E1
  | UO_Neg
  | UO_Flt
  | UO_Int
  | UO_Str
  | UO_Rev
  | UO_Pct
  | UO_CLZ
  | UO_CTZ
  | UO_Sin
  | UO_Cos
  | UO_Tan
  | UO_ASn
  | UO_ACs
  | UO_ATn
  | UO_Sinh
  | UO_Cosh
  | UO_Tanh
  | UO_ASnh
  | UO_ACsh
  | UO_ATnh
  | UO_Lg2
  | UO_Lg10
  | UO_Ln
  | UO_Sqrt
  | UO_Cbrt
  | UO_Exp
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



data FnTable = FnTable !(Map Int (Patn, Expr))

data VrTable = VrTable !(Map Int Obj)

data Interface = Interface !(Map Int [Int])
  deriving Show

data TypeMap
    = Struct   !Int
    | Union    !Int !(Set Int)
    | Class    !Int !(Map Int Interface)
    deriving Show
