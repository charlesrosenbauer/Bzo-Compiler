module BzoEmulator where
import Data.Text
import Data.Bits
import Data.Map.Strict










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
  |  Obj_Fault Text
  |  Obj_Expr Expr
  deriving Show



data Expr
  =  Exp_Binop Binop
  |  Exp_Unop  Unop
  |  Exp_Hof   HOF
  |  Exp_Func  Int
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



data FnTable = FnTable (Map Int (Patn, Expr))




--checkPattern :: Obj -> Patn -> Bool
--checkPattern




lispiter :: [Obj] -> [Obj] -> [Obj]
lispiter []  _ = []
lispiter xs [] = xs
lispiter ((Obj_Hole):xs) (y:ys) = y:(lispiter xs ys)
lispiter (x:xs) ys              = x:(lispiter xs ys)



apply :: FnTable -> Expr -> Obj -> Obj
apply ft (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a + b))
apply ft (Exp_Binop BO_Sub) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a - b))
apply ft (Exp_Binop BO_Mul) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a * b))
apply ft (Exp_Binop BO_Div) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `div` b))
apply ft (Exp_Binop BO_Mod) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `mod` b))

apply ft (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a + b))
apply ft (Exp_Binop BO_Sub) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a - b))
apply ft (Exp_Binop BO_Mul) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a * b))
apply ft (Exp_Binop BO_Div) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a / b))
--apply (Exp_Binop BO_Add) (Obj_Cmpd [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a `mod` b))

apply ft (Exp_Binop BO_Shl) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a   b ))
apply ft (Exp_Binop BO_Shr) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a (-b)))
apply ft (Exp_Binop BO_Rtl) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a   b ))
apply ft (Exp_Binop BO_Rtr) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a (-b)))
apply ft (Exp_Binop BO_Xor) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (xor a b))
apply ft (Exp_Binop BO_And) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .&. b))
apply ft (Exp_Binop BO_Or ) (Obj_Cmpd [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .|. b))

apply ft (Exp_Unop  UO_Pct) (Obj_Int x) = (Obj_Int (popCount x))
apply ft (Exp_Unop  UO_CTZ) (Obj_Int x) = (Obj_Int (countTrailingZeros x))
apply ft (Exp_Unop  UO_CLZ) (Obj_Int x) = (Obj_Int (countLeadingZeros  x))

apply ft (Exp_Join fs) obj = Prelude.foldr (apply ft) obj fs

apply ft (Exp_Lisp f xs) (Obj_Cmpd ys) = apply ft f $ Obj_Cmpd $ lispiter xs ys
apply ft (Exp_Lisp f xs) y             = apply ft f $ Obj_Cmpd $ lispiter xs [y]

apply ft (Exp_Map)           (Obj_Cmpd [(Obj_Expr f),    (Obj_Arr a b xs)]) = Obj_Arr a b $ Prelude.map   (apply ft f)   xs
apply ft (Exp_Hof HF_Map   ) (Obj_Cmpd [(Obj_Expr f),    (Obj_Arr a b xs)]) = Obj_Arr a b $ Prelude.map   (apply ft f)   xs
apply ft (Exp_Hof HF_Fold  ) (Obj_Cmpd [(Obj_Expr f), x, (Obj_Arr a b xs)]) =               Prelude.foldl (\x y -> apply ft f $ Obj_Cmpd [x, y]) x xs
apply ft (Exp_Hof HF_Scan  ) (Obj_Cmpd [(Obj_Expr f), x, (Obj_Arr a b xs)]) = Obj_Arr a b $ Prelude.scanl (\x y -> apply ft f $ Obj_Cmpd [x, y]) x xs

apply ft@(FnTable t) (Exp_Func f) x   = apply ft (snd $ t ! f) x

apply ft f (Obj_Cmpd [x]) = apply ft f x

apply ft f expr = Obj_Fault $ pack ("Error: missing case: " ++ (show f) ++ "\n:: applied to ::\n" ++ (show expr))
