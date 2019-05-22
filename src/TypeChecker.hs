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
checkType :: DefinitionTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> [BzoErr]
checkType _ (_, VoidType    _) (_, VoidType    _) = []
checkType _ (_, IntType  p i0) (_, IntType  _ i1) = ife (i0 == i1) [] [TypeErr p $ pack $ "Integer types " ++ (show i0) ++ " and " ++ (show i1) ++ " do not match."]
checkType _ (_, FltType  p f0) (_, FltType  _ f1) = ife (f0 == f1) [] [TypeErr p $ pack $ "Integer types " ++ (show f0) ++ " and " ++ (show f1) ++ " do not match."]
checkType _ (_, StrType  p s0) (_, StrType  _ s1) = ife (s0 == s1) [] [TypeErr p $ pack $ "Integer types " ++ (show s0) ++ " and " ++ (show s1) ++ " do not match."]

checkType d (h0,FuncType _ i0 o0) (h1,FuncType _ i1 o1) = (checkType d (h0, i0) (h1, i1)) ++ (checkType d (h0, o0) (h1, o1)) 

checkType d (_, LtrlType p t0) (_, LtrlType _ t1) =
  let
      -- TODO: Add extra cases for typeclass checking, subtype checking, etc.
  in ife (t0 == t1) [] [TypeErr p $ pack $ "Types " ++ (show $ getTyId d t0) ++ " and " ++ (show $ getTyId d t1) ++ " do not match."]

checkType d (h0,CmpdType p xs) (h1,CmpdType _ ys) =
  let
      xlen :: Int
      xlen = L.length xs

      ylen :: Int
      ylen = L.length ys

      samelength :: [BzoErr]
      samelength = ife (xlen == ylen) [] [TypeErr p $ pack $ "Expeted tuple with " ++ (show xlen) ++ " elements, found " ++ (show ylen) ++ "elements instead."]

      each :: [BzoErr]
      each = L.concatMap (\(a,b) -> checkType d (h0,a) (h1,b)) $ L.zip xs ys

      errs :: [BzoErr]
      errs = ife (L.null samelength) each samelength

  in errs

-- These two will probably spit out some gnarly error messages. Tech debt?
checkType d (h0, PolyType _ xs) (h1, PolyType p ys) = L.concatMap (\x -> checkType d (h0, x) (h1, PolyType p ys)) xs
checkType d (h0, t) (h1, PolyType _ ys) =
  let
      each :: [[BzoErr]]
      each = L.map (\y -> checkType d (h0, t) (h1, y)) ys

  in if (L.any L.null each)
      then []
      else L.concat each

checkType _ (_, x) (_, _) = [TypeErr (typos x) $ pack "Type Mismatch"]
