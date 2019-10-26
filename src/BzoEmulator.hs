{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2019 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

module BzoEmulator where
import BzoEmulatorTypes
import Data.Text
import Data.Bits
import Data.Map.Strict










noVars :: VrTable
noVars = VrTable Data.Map.Strict.empty

mergeVrTable :: VrTable -> VrTable -> VrTable
mergeVrTable (VrTable a) (VrTable b) = (VrTable (union a b))




--TODO: Add TVar checking. Not 100% sure yet how to do this.
checkPattern :: Obj -> Patn -> (Bool, VrTable)
checkPattern x              (Patn_Vr  v)     = (True, VrTable (Data.Map.Strict.singleton (fromIntegral v) x))
checkPattern (Obj_Int a)    (Patn_Int b)     = ((a == (fromIntegral b)),  noVars)
checkPattern (Obj_Flt a)    (Patn_Flt b)     = ((a == b), noVars)
checkPattern (Obj_Str a)    (Patn_Str b)     = ((a == b), noVars)
checkPattern (Obj_Bl  a)    (Patn_Bl  b)     = ((a == b), noVars)
checkPattern (Obj_Fnc a)    (Patn_Fnc b)     = ((a == b), noVars)
checkPattern (Obj_Nil)      (Patn_Nil)       = (True    , noVars)
checkPattern (Obj_Arr s xs) (Patn_Cmpd ys)   =
  let
      eqLen = s == (Prelude.length ys)
      pairs =  Prelude.map (\(x, y) -> checkPattern x y) $ Prelude.zip xs ys

      passes= Prelude.map fst pairs
      vrtabs= Prelude.map snd pairs

  in (eqLen && (and passes), Prelude.foldl mergeVrTable noVars vrtabs)

checkPattern x              (Patn_Poly xs)   = Prelude.head $ Prelude.filter fst $ Prelude.map (checkPattern x) xs
checkPattern (Obj_Typ a xs) (Patn_Typ  b []) = ((a == b), noVars)
checkPattern (Obj_Typ a xs) (Patn_Typ  b ys) =
  let
      eqTyp = a == b
      eqLen = (Prelude.length xs) == (Prelude.length ys)
      pairs =  Prelude.map (\(x, y) -> checkPattern x y) $ Prelude.zip xs ys

      passes= Prelude.map fst pairs
      vrtabs= Prelude.map snd pairs

  in (eqLen && eqTyp && (and passes), Prelude.foldl mergeVrTable noVars vrtabs)

checkPattern _              (Patn_Wild)      = (True, noVars)




lispiter :: [Obj] -> [Obj] -> [Obj]
lispiter []  _ = []
lispiter xs [] = xs
lispiter ((Obj_Hole):xs) (y:ys) = y:(lispiter xs ys)
lispiter (x:xs) ys              = x:(lispiter xs ys)



apply :: FnTable -> Expr -> Obj -> Obj
apply ft (Exp_Binop BO_Add) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a + b))
apply ft (Exp_Binop BO_Sub) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a - b))
apply ft (Exp_Binop BO_Mul) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a * b))
apply ft (Exp_Binop BO_Div) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `div` b))
apply ft (Exp_Binop BO_Mod) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a `mod` b))

apply ft (Exp_Binop BO_Add) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a + b))
apply ft (Exp_Binop BO_Sub) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a - b))
apply ft (Exp_Binop BO_Mul) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a * b))
apply ft (Exp_Binop BO_Div) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a / b))

apply ft (Exp_Binop BO_Log) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (logBase a b))
apply ft (Exp_Binop BO_Pow) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a ** b))
apply ft (Exp_Binop BO_Srd) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a ** (1 / b)))
--apply (Exp_Binop BO_Add) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Flt (a `mod` b))

apply ft (Exp_Binop BO_Shl) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a   b ))
apply ft (Exp_Binop BO_Shr) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (shift  a (-b)))
apply ft (Exp_Binop BO_Rtl) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a   b ))
apply ft (Exp_Binop BO_Rtr) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (rotate a (-b)))
apply ft (Exp_Binop BO_Xor) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (xor a b))
apply ft (Exp_Binop BO_And) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .&. b))
apply ft (Exp_Binop BO_Or ) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Int (a .|. b))

apply ft (Exp_Binop BO_And) (Obj_Arr 2 [(Obj_Bl  a), (Obj_Bl  b)]) = (Obj_Bl  (a && b))
apply ft (Exp_Binop BO_Or ) (Obj_Arr 2 [(Obj_Bl  a), (Obj_Bl  b)]) = (Obj_Bl  (a || b))

apply ft (Exp_Binop BO_Ls ) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a <  b))
apply ft (Exp_Binop BO_Gt ) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a >  b))
apply ft (Exp_Binop BO_Lse) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a <= b))
apply ft (Exp_Binop BO_Gte) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a >= b))
apply ft (Exp_Binop BO_Eq ) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a == b))
apply ft (Exp_Binop BO_NEq) (Obj_Arr 2 [(Obj_Int a), (Obj_Int b)]) = (Obj_Bl  (a /= b))

apply ft (Exp_Binop BO_Ls ) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a <  b))
apply ft (Exp_Binop BO_Gt ) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a >  b))
apply ft (Exp_Binop BO_Lse) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a <= b))
apply ft (Exp_Binop BO_Gte) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a >= b))
apply ft (Exp_Binop BO_Eq ) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a == b))
apply ft (Exp_Binop BO_NEq) (Obj_Arr 2 [(Obj_Flt a), (Obj_Flt b)]) = (Obj_Bl  (a /= b))

apply ft (Exp_Binop BO_Take   )(Obj_Arr 2 [(Obj_Int     a), (Obj_Arr  s xs)]) = (Obj_Arr (max a    s ) $ Prelude.take a xs)
apply ft (Exp_Binop BO_Drop   )(Obj_Arr 2 [(Obj_Int     a), (Obj_Arr  s xs)]) = (Obj_Arr (min 0 (s-a)) $ Prelude.drop a xs)
apply ft (Exp_Binop BO_Concat )(Obj_Arr 2 [(Obj_Arr s0 xs), (Obj_Arr s1 ys)]) = (Obj_Arr (s0+s1)       $ xs ++ ys)
apply ft (Exp_Binop BO_SplitAt)(Obj_Arr 2 [(Obj_Int     a), (Obj_Arr  s xs)]) =
  let (x,y) = Prelude.splitAt a xs
  in (Obj_Arr 2 [(Obj_Arr (Prelude.length x) x), (Obj_Arr (Prelude.length y) y)])

apply ft (Exp_Unop  UO_Reverse)(Obj_Arr s xs) = (Obj_Arr s (Prelude.reverse xs))
apply ft (Exp_Binop BO_Zip    )(Obj_Arr 2 [(Obj_Arr s0 as), (Obj_Arr s1 bs)]) =
  let
      combine :: Obj -> Obj -> Obj
      combine a b = (Obj_Arr 2 [a, b])
  in (Obj_Arr (min s0 s1) (Prelude.zipWith combine as bs))

apply ft (Exp_Binop BO_Zip    )(Obj_Arr 3 [(Obj_Arr s0 as), (Obj_Arr s1 bs), (Obj_Arr s2 cs)]) =
  let
      combine :: Obj -> Obj -> Obj -> Obj
      combine a b c = (Obj_Arr 3 [a, b, c])
  in (Obj_Arr (Prelude.minimum [s0, s1, s2]) (Prelude.zipWith3 combine as bs cs))

apply ft (Exp_Unop  UO_Concat )(Obj_Arr  s xs) =
  let
      getArr :: Obj -> [Obj]
      getArr (Obj_Arr _ os) = os

      xs' = Prelude.concatMap getArr xs
  in (Obj_Arr (Prelude.length xs') xs')

apply ft (Exp_Unop  UO_Not) (Obj_Int x) = (Obj_Int (complement x))
apply ft (Exp_Unop  UO_Neg) (Obj_Int x) = (Obj_Int (-x))
apply ft (Exp_Unop  UO_E0 ) (Obj_Int x) = (Obj_Bl  (x == 0))
apply ft (Exp_Unop  UO_E1 ) (Obj_Int x) = (Obj_Bl  (x == 1))

apply ft (Exp_Unop  UO_Neg) (Obj_Flt x) = (Obj_Flt (-x))
apply ft (Exp_Unop  UO_E0 ) (Obj_Flt x) = (Obj_Bl  (x == 0))
apply ft (Exp_Unop  UO_E1 ) (Obj_Flt x) = (Obj_Bl  (x == 1))

apply ft (Exp_Unop  UO_Sin) (Obj_Flt x) = (Obj_Flt $ sin   x)
apply ft (Exp_Unop  UO_Cos) (Obj_Flt x) = (Obj_Flt $ cos   x)
apply ft (Exp_Unop  UO_Tan) (Obj_Flt x) = (Obj_Flt $ tan   x)
apply ft (Exp_Unop  UO_ASn) (Obj_Flt x) = (Obj_Flt $ asin  x)
apply ft (Exp_Unop  UO_ACs) (Obj_Flt x) = (Obj_Flt $ acos  x)
apply ft (Exp_Unop  UO_ATn) (Obj_Flt x) = (Obj_Flt $ atan  x)
apply ft (Exp_Unop  UO_Sinh)(Obj_Flt x) = (Obj_Flt $ sinh  x)
apply ft (Exp_Unop  UO_Cosh)(Obj_Flt x) = (Obj_Flt $ cosh  x)
apply ft (Exp_Unop  UO_Tanh)(Obj_Flt x) = (Obj_Flt $ tanh  x)
apply ft (Exp_Unop  UO_ASnh)(Obj_Flt x) = (Obj_Flt $ asinh x)
apply ft (Exp_Unop  UO_ACsh)(Obj_Flt x) = (Obj_Flt $ acosh x)
apply ft (Exp_Unop  UO_ATnh)(Obj_Flt x) = (Obj_Flt $ atanh x)
apply ft (Exp_Unop  UO_Lg2) (Obj_Flt x) = (Obj_Flt $ logBase 2  x)
apply ft (Exp_Unop  UO_Lg10)(Obj_Flt x) = (Obj_Flt $ logBase 10 x)
apply ft (Exp_Unop  UO_Ln ) (Obj_Flt x) = (Obj_Flt $ logBase (exp 1) x)
apply ft (Exp_Unop  UO_Sqrt)(Obj_Flt x) = (Obj_Flt $ sqrt x)
apply ft (Exp_Unop  UO_Cbrt)(Obj_Flt x) = (Obj_Flt $ x ** (1/3))
apply ft (Exp_Unop  UO_Exp) (Obj_Flt x) = (Obj_Flt $ exp x)

apply ft (Exp_Unop  UO_Flt) (Obj_Int x) = (Obj_Flt $ fromIntegral x)
apply ft (Exp_Unop  UO_Int) (Obj_Flt x) = (Obj_Int $ truncate x)
apply ft (Exp_Unop  UO_Str) (Obj_Flt x) = (Obj_Str $ pack $ show x)
apply ft (Exp_Unop  UO_Str) (Obj_Int x) = (Obj_Str $ pack $ show x)

apply ft (Exp_Unop  UO_Pct) (Obj_Int x) = (Obj_Int (popCount x))
apply ft (Exp_Unop  UO_CTZ) (Obj_Int x) = (Obj_Int (countTrailingZeros x))
apply ft (Exp_Unop  UO_CLZ) (Obj_Int x) = (Obj_Int (countLeadingZeros  x))

apply ft (Exp_Join fs) obj = Prelude.foldr (apply ft) obj fs

apply ft (Exp_Lisp f xs) (Obj_Arr s ys) = apply ft f $ Obj_Arr s $ lispiter xs ys
apply ft (Exp_Lisp f xs) y              = apply ft f $
  let
      x = lispiter xs [y]
  in Obj_Arr (Prelude.length x) x

apply ft (Exp_Map)           (Obj_Arr 2 [(Obj_Expr f),    (Obj_Arr s xs)]) = Obj_Arr s $ Prelude.map   (apply ft f)   xs
apply ft (Exp_Hof HF_Map   ) (Obj_Arr 2 [(Obj_Expr f),    (Obj_Arr s xs)]) = Obj_Arr s $ Prelude.map   (apply ft f)   xs
apply ft (Exp_Hof HF_Fold  ) (Obj_Arr 2 [(Obj_Expr f), x, (Obj_Arr s xs)]) =             Prelude.foldl (\x y -> apply ft f $ Obj_Arr 2 [x, y]) x xs
apply ft (Exp_Hof HF_Scan  ) (Obj_Arr 2 [(Obj_Expr f), x, (Obj_Arr s xs)]) = Obj_Arr s $ Prelude.scanl (\x y -> apply ft f $ Obj_Arr 2 [x, y]) x xs
apply ft (Exp_Hof HF_Filter) (Obj_Arr 2 [(Obj_Expr f),    (Obj_Arr s xs)]) =
  let
      f' :: Obj -> Bool
      f' = (\x ->
              case (apply ft f x) of
                (Obj_Bl b) -> b
                _          -> False)
      ys = Prelude.filter f' xs
  in Obj_Arr (Prelude.length ys) ys

apply ft@(FnTable t) (Exp_Func f) x   = apply ft (snd $ t ! (fromIntegral f)) x

apply ft f (Obj_Arr 1 [x]) = apply ft f x

apply ft f expr = Obj_Fault $ pack ("Error: missing case: " ++ (show f) ++ "\n:: applied to ::\n" ++ (show expr))
