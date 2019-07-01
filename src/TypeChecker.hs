module TypeChecker where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import Data.Either as E
import Query
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tuple as Tp
import Control.Parallel.Strategies
import Debug.Trace










getTyId :: DefinitionTable -> Int64 -> Text
getTyId (DefinitionTable defs _ _ _) t = identifier $ defs M.! t

{-
  TODO:
    * Subtyping checks
    * Typeclass checks
    * Value checks for ints, flts, and strings
    * Maketype checks
    * TVar types
    * Function literal checks
    * Array type checks
    * Tuple-array casting
    * Builtin checks
-}
data IOKind = InKind | ExKind deriving Eq

checkXS_Y  :: IOKind -> DefinitionTable -> (TypeHeader, [Type]) -> (TypeHeader, Type) -> ([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])
checkXS_Y  k d (h0, ts) (h1, t1) = concatUnzip3 $ L.map (\t -> checkType' k d (h0,t) (h1,t1)) ts

checkX_YS  :: IOKind -> DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, [Type]) -> ([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])
checkX_YS  k d (h0, t0) (h1, ts) = concatUnzip3 $ L.map (\t -> checkType' k d (h0,t0) (h1,t)) ts

checkXS_YS :: BzoPos -> IOKind -> DefinitionTable -> (TypeHeader, [Type]) -> (TypeHeader, [Type]) -> ([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])
checkXS_YS p k d (h0, xs) (h1, ys) =
  let
      xlen :: Int
      xlen = L.length xs

      ylen :: Int
      ylen = L.length ys

      samelength :: [BzoErr]
      samelength = ife (xlen == ylen) [] [TypeErr p $ pack $ "Expeted tuple with " ++ (show xlen) ++ " elements, found " ++ (show ylen) ++ " elements instead."]

      eacherr :: [BzoErr]
      eachvar :: [(TVId, Type, IOKind)]
      eachtc  :: [(TypeHeader, Type, TCId)]
      (eacherr, eachvar, eachtc) = concatUnzip3 $ L.map (\(a,b) -> checkType' k d (h0,a) (h1,b)) $ L.zip xs ys

      errs :: [BzoErr]
      errs = ife (L.null samelength) eacherr samelength

  in (errs, eachvar, eachtc)










checkConstraints :: DefinitionTable -> TypeHeader -> (TypeHeader, Type) -> [BzoErr]
checkConstraints dt h0@(TyHeader [hd] hmap) (h1, t) =
  let
      checkConstraint :: THeadAtom -> [BzoErr]
      checkConstraint (TVrAtom p v cs) = L.concatMap (\(Constraint _ c) -> checkType dt (h0, t) (h1, c)) cs

  in  checkConstraint $ hmap M.! hd

checkConstraints dt h0@(TyHeader  hd  hmap) (h1, CmpdType p xs) =
  let
      hlen :: Int
      hlen = L.length hd

      xlen :: Int
      xlen = L.length xs

      pairs :: [(THeadAtom, Type)]
      pairs = L.map (\(h,t) -> (hmap M.! h, t)) $ L.zip hd xs

      checkPair :: (THeadAtom, Type) -> [BzoErr]
      checkPair (TVrAtom p v cs, t) = L.concatMap (\(Constraint _ c) -> checkType dt (h0, t) (h1, c)) cs

  in if (hlen == xlen)
      then L.concatMap checkPair pairs
      else [TypeErr p $ pack $ "Cannot construct type. Expected " ++ (show hlen) ++ " parameters, found " ++ (show xlen) ++ ".\n" ++ (show h0) ++ "\n&\n" ++ (show (CmpdType p xs))]

checkConstraints dt h0 (h1, MakeType p [t]) = checkConstraints dt h0 (h1, t)

checkConstraints dt h0 (h1, t) = checkConstraints dt h0 (h1, CmpdType (typos t) [t])










{-
  A <= B
-}
checkType' :: IOKind -> DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> ([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])

-- Primitive Type Checking
checkType' _ _ (_, VoidType    _) (_, VoidType    _) = ([], [], [])
checkType' _ _ (_, IntType  p i0) (_, IntType  _ i1) = (ife (i0 == i1) [] [TypeErr p $ pack $ "Integer literals "   ++ (show i0) ++ " and " ++ (show i1) ++ " do not match."], [], [])
checkType' _ _ (_, FltType  p f0) (_, FltType  _ f1) = (ife (f0 == f1) [] [TypeErr p $ pack $ "Float literals "     ++ (show f0) ++ " and " ++ (show f1) ++ " do not match."], [], [])
checkType' _ _ (_, StrType  p s0) (_, StrType  _ s1) = (ife (s0 == s1) [] [TypeErr p $ pack $ "String literals "    ++ (show s0) ++ " and " ++ (show s1) ++ " do not match."], [], [])
checkType' _ _ (_, FLitType p s0) (_, FLitType _ s1) = (ife (s0 == s1) [] [TypeErr p $ pack $ "Function literals "  ++ (show s0) ++ " and " ++ (show s1) ++ " do not match."], [], [])
checkType' _ _ (_, IntType  p i0) (_, BITyType _  b) = (ife (b  == 12) [] [TypeErr p $ pack $ "Expected int builtin"      ], [], [])
checkType' _ _ (_, FltType  p f0) (_, BITyType _  b) = (ife (b  == 13) [] [TypeErr p $ pack $ "Expected float builtin"    ], [], [])
checkType' _ _ (_, StrType  p s0) (_, BITyType _  b) = (ife (b  == 17) [] [TypeErr p $ pack $ "Expected string builtin"   ], [], [])
checkType' _ _ (_, BITyType p b0) (_, BITyType _ b1) = (ife (b0 == b1) [] [TypeErr p $ pack $ "Builtin types do not match"], [], [])


-- Primitive Literal Checking
checkType' k d (h0,LtrlType p0 t0) (h1,LtrlType p t1) =
  let
      istc = isTyClass $ (dt_defs d) M.! t0

      tdef :: Definition
      tdef = (dt_defs d) M.! t0

      h2 :: TypeHeader
      h2 = typehead tdef

      t2 :: Type
      t2 = typedef  tdef

  in case (istc, t0 == t1, checkType' k d (h2, t2) (h1, LtrlType p t1)) of
      (True,  _    , _          ) -> ([], [], [(h1, LtrlType p t1, t0)])
      (False, True , (_ , _, tc)) -> ([], [], tc)
      (False, False, ([], _, tc)) -> ([], [], tc)
      (False, False, (er, _, tc)) -> ([TypeErr p0 $ pack $ "Types " ++ (show $ getTyId d t0) ++ " and " ++ (show $ getTyId d t1) ++ " do not match."] ++ er, [], [])


-- Array Type Checking
checkType' k d (h0, ArryType p 0  _ ) (h1, ArryType _ _ _) = ([TypeErr p $ pack $ "Cannot constrain array size."], [], [])

checkType' k d (h0, ArryType _ _  t0) (h1, ArryType _ 0  t1) = checkType' k d (h0, t0) (h1, t1)

checkType' k d (h0, ArryType p s0 t0) (h1, ArryType _ s1 t1) =
  ife (s0 /= s1)
    ([TypeErr p $ pack $ "Expected array of size " ++ (show s1) ++ ", found one of size " ++ (show s0) ++ "."], [], [])
    (checkType' k d (h0, t0) (h1, t1))


-- Compound Type Checking
checkType' k d (h0, CmpdType p   ts0) (h1, ArryType _ 0 t1) = checkXS_Y k d (h0, ts0) (h1, t1)

checkType' k d (h0, CmpdType p   ts0) (h1, ArryType _ s t1) =
  ife (L.length ts0 /= fromIntegral s)
    ([TypeErr p $ pack $ "Casting Tuple to Array failed; expected " ++ (show s) ++ " elements, found " ++ (show $ L.length ts0) ++ "."], [], [])
    (checkXS_Y k d (h0, ts0) (h1, t1))

checkType' k d (h0,CmpdType p xs) (h1,CmpdType _ ys) = checkXS_YS p k d (h0, xs) (h1, ys)


-- Function Type Checking
checkType' _ d (h0,FuncType _ i0 o0) (h1,FuncType _ i1 o1) =
  let
      inErrs :: [BzoErr]
      inVars :: [(TVId, Type, IOKind)]
      inTcs  :: [(TypeHeader, Type, TCId)]
      (inErrs, inVars, inTcs) = (checkType' InKind d (h0, i0) (h1, i1))

      exErrs :: [BzoErr]
      exVars :: [(TVId, Type, IOKind)]
      exTcs  :: [(TypeHeader, Type, TCId)]
      (exErrs, exVars, exTcs) = (checkType' ExKind d (h0, o0) (h1, o1))

  in ((inErrs ++ exErrs), (inVars ++ exVars), (inTcs ++ exTcs))


-- Polymorphic Type Checking
-- These will probably spit out some gnarly error messages. Tech debt?
checkType' k d (h0, PolyType p xs) (h1, PolyType _ ys) =
  let
      each :: [([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])]
      each = L.map (\x -> checkType' k d (h0, x) (h1, PolyType p ys)) xs

      eacherrs :: [Either [BzoErr] ([(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])]
      eacherrs = L.map (\(xs,ys,zs) -> case xs of
                                      [] -> Right (ys, zs)
                                      _  -> Left  xs) each

      eachvars :: [(TVId, Type, IOKind)]
      eachvars = L.concat $ L.map fst $ rights eacherrs

      eachtcs  :: [(TypeHeader, Type, TCId)]
      eachtcs  = L.concat $ L.map snd $ rights eacherrs

  in  if (L.any E.isRight eacherrs)
        then ([], eachvars, eachtcs)
        else (L.concat $ lefts eacherrs, [], [])

checkType' k d (h0, PolyType _ [t0]) (h1, t1) = checkType' k d (h0, t0) (h1, t1)

checkType' k d (h0, t0) (h1, PolyType _ [t1]) = checkType' k d (h0, t0) (h1, t1)

checkType' k d (h0, t) (h1, PolyType _ ys) =
  let
      -- Vars should be filtered to only those with no corresponding errors.
      each :: [([BzoErr], [(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])]
      each = L.map (\y -> checkType' k d (h0, t) (h1, y)) ys

      eacherrs :: [Either [BzoErr] ([(TVId, Type, IOKind)], [(TypeHeader, Type, TCId)])]
      eacherrs = L.map (\(xs,ys,zs) -> case xs of
                                      [] -> Right (ys, zs)
                                      _  -> Left  xs) each

      eachvars :: [(TVId, Type, IOKind)]
      eachvars = L.concat $ L.map fst $ rights eacherrs

      eachtcs  :: [(TypeHeader, Type, TCId)]
      eachtcs  = L.concat $ L.map snd $ rights eacherrs

  in if (L.any E.isRight eacherrs)
      then ([], eachvars, eachtcs)
      else (L.concat $ lefts eacherrs, [], [])


-- Type Composition Checking
checkType' k d (h0, MakeType _ []) (h1, MakeType _ []) = ([], [], [])

checkType' k d (h0, MakeType p []) (h1, MakeType _  _) = ([TypeErr p $ pack "Type mismatch; expected more parameters."], [], [])

checkType' k d (h0, MakeType p  _) (h1, MakeType _ []) = ([TypeErr p $ pack "Type mismatch; expected fewer parameters."], [], [])

checkType' k d (h0, MakeType p0 (x:xs)) (h1, MakeType p1 (y:ys)) =
  case (checkType' k d (h0, x) (h1, y)) of
    ([], [], []) -> checkType' k d (h0, MakeType p0 xs) (h1, MakeType p1 ys)  -- Probably not the best way to track positions here.
    ret          -> ret

checkType' k d (h0, MakeType p [x]) (h1, y) = checkType' k d (h0, x) (h1, y)

checkType' k d (h0, x) (h1, MakeType p [y]) = checkType' k d (h0, x) (h1, y)


-- Complex Type Literal Checking
checkType' k d (h0, t0) (h1,LtrlType _ t1) =
  let
      tdef :: Definition
      tdef = (dt_defs d) M.! t1

      h2 :: TypeHeader
      h2 = typehead tdef

      t2 :: Type
      t2 = typedef  tdef

  in case (checkType' k d (h0, t0) (h2, t2)) of
      ([], _ , tc) -> ([], [], tc)
      (er, _ , _ ) -> ([TypeErr (typos t0) $ pack ("Could not match type:\n" ++ (show t0) ++ "\non type " ++ (show $ getTyId d t1) ++ "\n")] ++ er, [], [])

checkType' k d (h0,LtrlType p t0) (h1, t1) =
  let
      istc = isTyClass $ (dt_defs d) M.! t0

      tdef :: Definition
      tdef = (dt_defs d) M.! t0

      h2 :: TypeHeader
      h2 = typehead tdef

      t2 :: Type
      t2 = typedef  tdef

  in case (istc, checkType' k d (h2, t2) (h1, t1)) of
      (True,  (er, vs, tc)) -> (er, vs, tc ++ [(h1, t1, t0)])
      (False, (er, _,  tc)) -> ([TypeErr p $ pack ("Could not match type " ++ (show $ getTyId d t0) ++ "\non type:\n" ++ (show t1) ++ "\n")] ++ er, [], [])


-- Type Variable Checking
checkType' k d (h0, t) (h1, TVarType p v) = ([], [(v, t, k)], [])


-- Fallthrough Case
checkType' _ _ (_, x) (_, y) = ([TypeErr (typos y) $ pack ("Type Mismatch:\n" ++ (show x) ++ "\n&&\n" ++ (show y))], [], [])










--TODO: !!TEST THIS CODE!!
checkWithVars :: DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> ([BzoErr], M.Map TVId Type, [(TypeHeader, Type, TCId)])
checkWithVars d a@(h0,t0) b@(h1,t1) =
  let
      errs :: [BzoErr]
      vals :: [(TVId, Type, IOKind)]
      tcs  :: [(TypeHeader, Type, TCId)]
      (errs, vals, tcs) = checkType' InKind d a b

      -- Not actually necessary, but this will probably improve cache performance
      -- in later code. Haskell has AWFUL cache performance, so this will probably
      -- help a lot.
      tcs' = L.concat $ L.groupBy (\a b  -> (trd3 a) == (trd3 b)) tcs

      groupVars :: [(TVId, Type, IOKind)] -> [[(TVId, Type, IOKind)]]
      groupVars xs = L.groupBy (\(v0,_,_)(v1,_,_) -> v0 == v1) $ L.sortBy (\(v0,_,_)(v1,_,_) -> compare v0 v1) xs

      ins :: [[(TVId, Type, IOKind)]]
      exs :: [[(TVId, Type, IOKind)]]
      (ins, exs) = (\(a,b) -> (groupVars a, groupVars b)) $ L.partition (\(_,_,k) -> k==InKind) vals

      ts :: M.Map TVId Type
      ts = M.fromList $ L.map (\((v,t,_):xs) -> (v,t)) ins

      -- This might not be good enough, but should be fine for now
      testMatch :: [(TVId, Type, IOKind)] -> [BzoErr]
      testMatch xs =
        let
            vtyp :: Type
            vtyp = ts M.! (fst3 $ L.head xs)

            nonmatches :: [(TVId, Type, IOKind)]
            nonmatches = L.filter (\(v, t', _) -> t' /= vtyp) $ L.tail xs
        in  if (L.null nonmatches)
              then []
              else [TypeErr (typos vtyp) $ pack "Tyvars do not match."]  -- TODO: get a better error message

      testSubtype :: [(TVId, Type, IOKind)] -> [BzoErr]
      testSubtype xs =
        let
            vtyp :: Type
            vtyp = ts M.! (fst3 $ L.head xs)

            errs :: [[BzoErr]]
            --vals :: [[(TVId, Type, IOKind)]]
            (errs, _, _) = L.unzip3 $ L.map (\(_,x,_) -> checkType' ExKind d (h1, vtyp) (h1, x)) xs
        in  if (L.null $ L.concat errs)
              then []
              else [TypeErr (typos vtyp) $ pack "Output type is not a subtype of prototype"]

      errs' :: [BzoErr]
      errs' = errs -- ++ otherErrs

      -- TODO: go through this more thoroughly
      tvpairs :: M.Map TVId Type
      tvpairs = M.fromList $ L.map dtrd3 $ L.concat ins

  in case errs' of
      [] -> ([], tvpairs, tcs')
      er -> (er, tvpairs, tcs')

checkType :: DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> [BzoErr]
checkType dt a b = fst3 $ checkWithVars dt a b










validType :: DefinitionTable -> (TypeHeader, Type) -> [BzoErr]
validType dt (h, CmpdType _  ts) = L.concatMap (\t -> validType dt (h, t)) ts
validType dt (h, PolyType _  ts) = L.concatMap (\t -> validType dt (h, t)) ts
validType dt (h, FuncType _ i o) = (validType dt (h, i)) ++ (validType dt (h, o))
validType dt (h, ArryType _ _ t) =  validType dt (h, t)
validType dt (h, MakeType _ [t]) =  validType dt (h, t)
validType dt (h, MakeType _  []) = []
validType dt (h, MakeType _ ((CmpdType p xs):t:ts)) = [TypeErr p $ pack "Unexpected Compound in Type Definition."]
validType dt (h, MakeType p ((LtrlType _  t):ts)) =
  let
      terr :: [BzoErr]
      terr = validType dt (h, MakeType p ts)

      thead  :: TypeHeader
      typ    :: Type
      (thead, typ) = (\t -> (typehead t, typedef t)) ((dt_defs dt) M.! t)

      cerr = checkConstraints dt thead (h, MakeType p ts)
  in terr ++ cerr

validType dt (h, t) = []











validateTypePass :: DefinitionTable -> [BzoErr]
validateTypePass dt@(DefinitionTable defs files ids top) =
  let
      tys :: [(TypeHeader, Type)]
      tys = L.map (\t -> (typehead t, typedef t)) $ L.filter isTyDef $ M.elems defs

  in L.concatMap (validType dt) tys










testTypeCheck :: DefinitionTable -> [BzoErr]
testTypeCheck dt =
  let
      p1 = (\x -> BzoPos x 0 $ pack "Test-Inpos")
      p2 = (\x -> BzoPos x 0 $ pack "Test-Expos")

      err0 = checkType dt (TyHeader [] M.empty, CmpdType (p1 0) [VoidType (p1 0)]) (TyHeader [] M.empty, CmpdType (p2 0) [VoidType (p2 0)])
      err1 = checkType dt (TyHeader [] M.empty, PolyType (p1 1) [VoidType (p1 1)]) (TyHeader [] M.empty, PolyType (p2 1) [VoidType (p2 1), VoidType (p2 1)])
      err2 = checkType dt (TyHeader [] M.empty, PolyType (p1 2) [VoidType (p1 2)]) (TyHeader [] M.empty, PolyType (p2 2) [VoidType (p2 2), PolyType (p2 2) [IntType (p2 2) 0, IntType (p2 2) 2]])

      -- These next two should produce errors
      err3 = checkType dt (TyHeader [] M.empty, PolyType (p1 3) [VoidType (p1 3), PolyType (p1 3) [IntType (p1 3) 0, IntType (p1 3) 3]]) (TyHeader [] M.empty, PolyType (p2 3) [VoidType (p2 3)])
      err4 = checkType dt (TyHeader [] M.empty, PolyType (p1 4) [VoidType (p1 4), IntType  (p1 4) 0, IntType (p1 4) 3]) (TyHeader [] M.empty, PolyType (p2 4) [VoidType (p2 4)])

      err5 = checkType dt (TyHeader [] M.empty, CmpdType (p1 5) [IntType (p1 5) 0, IntType (p1 5) 1]) (TyHeader [] M.empty, ArryType (p2 5) 3 (BITyType (p2 5) 12))
  in err0 ++ err1 ++ err2 ++ err3 ++ err4 ++ err5
