module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.Maybe
import Data.List hiding (map, foldl, foldr, insert)
import Data.Map.Strict hiding (map, foldl, foldr, mapEither)
import Data.Set hiding (map, foldl, foldr, mapEither, empty)
import HigherOrder
import Debug.Trace










data BzoLiteral
  = LtStr String
  | LtInt Integer
  | LtFlt Double
  | LtId  String
  | LtFn  String
  | LtTy  String
  | LtTV  String
  | LtVr  String
  | LtNm  BzoLiteral String
  | LtArr BzoLiteral BzoType
  | LtNms BzoLiteral String










data BzoType
  = TyCmpd [BzoType]
  | TyPoly [BzoType]
  | TyLit   BzoLiteral
  | TyFnDf  BzoType BzoType
  | TyFilt  BzoType BzoType
  | TyExpr  BzoType BzoType
  | TyWild
  | TyNil
