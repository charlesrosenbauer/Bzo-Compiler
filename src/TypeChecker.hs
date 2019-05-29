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

checkType' :: IOKind -> DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> ([BzoErr], [(TVId, Type, IOKind)])
checkType' _ _ (_, VoidType    _) (_, VoidType    _) = ([], [])
checkType' _ _ (_, IntType  p i0) (_, IntType  _ i1) = (ife (i0 == i1) [] [TypeErr p $ pack $ "Integer types " ++ (show i0) ++ " and " ++ (show i1) ++ " do not match."], [])
checkType' _ _ (_, FltType  p f0) (_, FltType  _ f1) = (ife (f0 == f1) [] [TypeErr p $ pack $ "Integer types " ++ (show f0) ++ " and " ++ (show f1) ++ " do not match."], [])
checkType' _ _ (_, StrType  p s0) (_, StrType  _ s1) = (ife (s0 == s1) [] [TypeErr p $ pack $ "Integer types " ++ (show s0) ++ " and " ++ (show s1) ++ " do not match."], [])

checkType' _ d (h0,FuncType _ i0 o0) (h1,FuncType _ i1 o1) =
  let
      inErrs :: [BzoErr]
      inVars :: [(TVId, Type, IOKind)]
      (inErrs, inVars) = (checkType' InKind d (h0, i0) (h1, i1))

      exErrs :: [BzoErr]
      exVars :: [(TVId, Type, IOKind)]
      (exErrs, exVars) = (checkType' ExKind d (h0, o0) (h1, o1))

  in ((inErrs ++ exErrs), (inVars ++ exVars))

checkType' _ d (_, LtrlType p t0) (_, LtrlType _ t1) =
  let
      -- TODO: Add extra cases for typeclass checking, subtype checking, etc.
  in (ife (t0 == t1) [] [TypeErr p $ pack $ "Types " ++ (show $ getTyId d t0) ++ " and " ++ (show $ getTyId d t1) ++ " do not match."], [])

checkType' k d (h0,CmpdType p xs) (h1,CmpdType _ ys) =
  let
      xlen :: Int
      xlen = L.length xs

      ylen :: Int
      ylen = L.length ys

      samelength :: [BzoErr]
      samelength = ife (xlen == ylen) [] [TypeErr p $ pack $ "Expeted tuple with " ++ (show xlen) ++ " elements, found " ++ (show ylen) ++ " elements instead."]

      eacherr :: [BzoErr]
      eachvar :: [(TVId, Type, IOKind)]
      (eacherr, eachvar) = concatUnzip $ L.map (\(a,b) -> checkType' k d (h0,a) (h1,b)) $ L.zip xs ys

      errs :: [BzoErr]
      errs = ife (L.null samelength) eacherr samelength

  in (errs, eachvar)

-- These two will probably spit out some gnarly error messages. Tech debt?
checkType' k d (h0, PolyType _ xs) (h1, PolyType p ys) =
  let
      each :: [([BzoErr], [(TVId, Type, IOKind)])]
      each = L.map (\x -> checkType' k d (h0, x) (h1, PolyType p ys)) xs

      eacherrs :: [Either [BzoErr] [(TVId, Type, IOKind)]]
      eacherrs = L.map (\(xs,ys) -> case xs of
                                      [] -> Right ys
                                      _  -> Left  xs) each

  in  if (L.any E.isRight eacherrs)
        then ([], L.concat $ rights eacherrs)
        else (L.concat $ lefts eacherrs, [])

checkType' k d (h0, t) (h1, PolyType _ ys) =
  let
      -- Vars should be filtered to only those with no corresponding errors.
      each :: [([BzoErr], [(TVId, Type, IOKind)])]
      each = L.map (\y -> checkType' k d (h0, t) (h1, y)) ys

      eacherrs :: [Either [BzoErr] [(TVId, Type, IOKind)]]
      eacherrs = L.map (\(xs,ys) -> case xs of
                                      [] -> Right ys
                                      _  -> Left  xs) each

  in if (L.any E.isRight eacherrs)
      then ([], L.concat $ rights eacherrs)
      else (L.concat $ lefts eacherrs, [])

checkType' k d (h0, TVarType p v) (h1, t) = ([], [(v, t, k)])

checkType' _ _ (_, x) (_, _) = ([TypeErr (typos x) $ pack "Type Mismatch"], [])










--TODO: !!TEST THIS CODE!!
checkType :: DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> [BzoErr]
checkType d a@(h0,t0) b@(h1,t1) =
  let
      errs :: [BzoErr]
      vals :: [(TVId, Type, IOKind)]
      (errs, vals) = checkType' InKind d a b

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
            (errs, _) = L.unzip $ L.map (\(_,x,_) -> checkType' ExKind d (h1, vtyp) (h1, x)) xs
        in  if (L.null $ L.concat errs)
              then []
              else [TypeErr (typos vtyp) $ pack "Output type is not a subtype of prototype"]

      errs' :: [BzoErr]
      errs' = errs -- ++ otherErrs
  in case errs' of
      [] -> []
      er -> er










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
  in err0 ++ err1 ++ err2 ++ err3
