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

module BzoTypes where
import Data.Int
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import Tokens
import AST
import Error
import HigherOrder












type FnId = Int64   -- Function Id
type TyId = Int64   -- Type Id
type TCId = Int64   -- Type Class Id
type VrId = Int64   -- Variable Id
type TVId = Int64   -- Type Variable Id
type LcId = Int64   -- Local Id










data TypeHeader = TyHeader { header :: ![TVId], tvarmap :: !(M.Map TVId THeadAtom) } deriving (Eq, Show)

getConstraints :: TypeHeader -> TVId -> [Constraint]
getConstraints (TyHeader _ vmap) tv =
  case M.lookup tv vmap of
    Just atm -> tatomcns atm
    Nothing  -> []

emptyheader :: TypeHeader
emptyheader = TyHeader [] M.empty

bumpHeader :: TVId -> TypeHeader -> TypeHeader
bumpHeader n (TyHeader vs vmap) =
  let
      vs' :: [TVId]
      vs' = L.map (+n) vs

      bumpConstraint :: Constraint -> Constraint
      bumpConstraint (Constraint p t) = (Constraint p (bumpTVs n t))

      bumpTAtom      :: THeadAtom -> THeadAtom
      bumpTAtom (TVrAtom p tid cs) = (TVrAtom p tid (L.map bumpConstraint cs))

      vmap' :: M.Map TVId THeadAtom
      vmap' = M.fromList $ L.map (\(k,v)-> (k+n, bumpTAtom v)) $ M.assocs vmap

  in (TyHeader vs' vmap')

data Type
  = UnresType !BzoSyntax
  | ParamType !BzoPos !BzoSyntax
  | FuncType  !BzoPos !Type !Type
  | CmpdType  !BzoPos ![Type]
  | PolyType  !BzoPos ![Type]
  | MakeType  !BzoPos ![Type]
  | IntType   !BzoPos !Integer
  | FltType   !BzoPos !Double
  | StrType   !BzoPos !T.Text
  | VoidType  !BzoPos
  | LtrlType  !BzoPos !TyId
  | TVarType  !BzoPos !TVId
  | BITyType  !BzoPos !TyId
  | ArryType  !BzoPos !Integer !Type
  | FLitType  !BzoPos ![FnId]
  | TCType    !BzoPos !TCId    -- Carrying around the interface is useful, but raises potential recursion problems.
  | InvalidType

eqType :: Type -> Type -> Bool
eqType (UnresType      _) (UnresType      _) = True
eqType (ParamType    _ a) (ParamType    _ b) = (a == b)
eqType (FuncType _ i0 o0) (FuncType _ i1 o1) = (i0 == i1) && (o0 == o1)
eqType (CmpdType _    xs) (CmpdType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (PolyType _    xs) (PolyType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (MakeType _    xs) (PolyType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (IntType  _     a) (IntType  _     b) = (a == b)
eqType (FltType  _     a) (FltType  _     b) = (a == b)
eqType (StrType  _     a) (StrType  _     b) = (a == b)
eqType (VoidType _      ) (VoidType _      ) = True
eqType (LtrlType _    t0) (LtrlType _    t1) = (t0 == t1)
eqType (TVarType _    v0) (TVarType _    v1) = (v0 == v1)
eqType (BITyType _    b0) (BITyType _    b1) = (b0 == b1)
eqType (ArryType _ i0 t0) (ArryType _ i1 t1) = (i0 == i1) && (t0 == t1)
eqType (FLitType _    f0) (FLitType _    f1) = ((L.sort f0) == (L.sort f1))
eqType (TCType   _    i0) (TCType   _    i1) = (i0 == i1)
eqType InvalidType        InvalidType        = True
eqType _ _ = False
instance Eq Type where (==) = eqType
                       (/=) = (\a b-> not $ eqType a b)

typos :: Type -> BzoPos
typos (UnresType   ast) = pos ast
typos (ParamType p _  ) = p
typos (FuncType  p _ _) = p
typos (CmpdType  p   _) = p
typos (PolyType  p   _) = p
typos (MakeType  p   _) = p
typos (IntType   p   _) = p
typos (FltType   p   _) = p
typos (StrType   p   _) = p
typos (VoidType  p    ) = p
typos (LtrlType  p   _) = p
typos (TVarType  p   _) = p
typos (BITyType  p   _) = p
typos (ArryType  p _ _) = p
typos (FLitType  p   _) = p
typos (TCType    p   _) = p

bumpTVs :: TVId -> Type -> Type
bumpTVs n (TVarType p   x) = (TVarType p (x+n))
bumpTVs n (CmpdType p  xs) = (CmpdType p   (L.map (bumpTVs n) xs))
bumpTVs n (PolyType p  xs) = (PolyType p   (L.map (bumpTVs n) xs))
bumpTVs n (MakeType p  xs) = (MakeType p   (L.map (bumpTVs n) xs))
bumpTVs n (ArryType p s x) = (ArryType p s (bumpTVs n x))
bumpTVs n (FuncType p i o) = (FuncType p   (bumpTVs n i) (bumpTVs n o))
bumpTVs n t = t

addConstraint :: BzoPos -> TVId -> TypeHeader -> Type -> TypeHeader
addConstraint p tvid (TyHeader hd tvs) ty =
  let
      ta :: THeadAtom
      ta = tvs M.! tvid

      cn :: Constraint
      cn = Constraint p ty

      ta':: THeadAtom
      ta'= (TVrAtom (tatompos ta) (tatomvar ta) (cn : (tatomcns ta)))

  in (TyHeader hd (M.insert tvid ta' tvs))

-- Replace tvid in t0 with t1
fuseTypes :: BzoPos -> TVId -> (TypeHeader, Type) -> (TypeHeader, Type) -> (TypeHeader, Type)
fuseTypes p tvid (th0, t0) (th1, t1) =
  let
      top :: TVId
      top = 1 + (L.maximum (0:(header th0)))

      th2 :: TypeHeader
      th2 = bumpHeader top th1

      t2  :: Type
      t2  = bumpTVs    top t1

  in  (addConstraint p tvid th0 t2, t0)










showType :: Type -> String
showType (UnresType     ast) = "[UnresType " ++ (show ast) ++ "]"
showType (ParamType _   pat) = "[ParamType " ++ (show pat) ++ "]"
showType (FuncType  _ ta tb) = "[FuncType "  ++ (show  ta) ++ " " ++ (show tb) ++ "]"
showType (CmpdType  _    ts) = "[CmpdType "  ++ (L.concat $ L.intersperse " , "  $ L.reverse $ L.map show ts) ++ "]"
showType (PolyType  _    ts) = "[PolyType "  ++ (L.concat $ L.intersperse " , "  $ L.reverse $ L.map show ts) ++ "]"
showType (MakeType  _    ts) = "[MakeType "  ++ (L.concat $ L.intersperse " >> " $ L.reverse $ L.map show ts) ++ "]"
showType (IntType   _     i) = "[IntLit "    ++ (show   i) ++ "]"
showType (FltType   _     f) = "[FltLit "    ++ (show   f) ++ "]"
showType (StrType   _     s) = "[StrLit "    ++ (show   s) ++ "]"
showType (VoidType  _      ) = "[VoidType]"
showType (LtrlType  _    ty) = "TY:" ++ (show ty)
showType (TVarType  _    tv) = "TV:" ++ (show tv)
showType (BITyType  _    bt) = "BT:" ++ (show bt)
showType (ArryType  _  i ty) = "[ArrayType [" ++ (show i) ++ "] " ++ (show ty) ++ "]"
showType (FLitType  _    fn) = "FN:" ++ (show fn)
showType (TCType    _    tc) = "TC:" ++ (show tc)
showType (InvalidType      ) = "INVALIDTYPE"
instance Show Type where show = showType










data Constraint = Constraint !BzoPos !Type  deriving Eq

showConstraint :: Constraint -> String
showConstraint (Constraint p t) = "[Constraint: " ++ (show t) ++ "]"
instance Show Constraint where show = showConstraint










data THeadAtom
  = TVrAtom !BzoPos !T.Text ![Constraint]
  deriving Eq

tatompos :: THeadAtom -> BzoPos
tatompos (TVrAtom p t cs) = p

tatomvar :: THeadAtom -> T.Text
tatomvar (TVrAtom p t cs) = t

tatomcns :: THeadAtom -> [Constraint]
tatomcns (TVrAtom p t cs) = cs

showTHead :: THeadAtom -> String
showTHead (TVrAtom _ k cs) = "[" ++ (show k) ++ " . " ++ (show cs) ++ "]"
instance Show THeadAtom where show = showTHead

atomId :: THeadAtom -> T.Text
atomId (TVrAtom _ t _) = t
