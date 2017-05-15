module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import Data.List hiding (map, foldl)
import Data.Map.Strict hiding (map, foldl)










data BzoLiteral
  = LtStr String
  | LtInt Integer
  | LtFlt Double
  | LtId  String
  | LtFn  String
  | LtTy  String
  | LtTV  String
  | LtVr  String
  | LtArr BzoLiteral BzoLiteral










data BzoType
  = TyCmpd [BzoType]
  | TyPoly [BzoType]
  | TyLit   BzoLiteral
  | TyFnDf  BzoType BzoType
  | TyFilt  BzoType BzoType
  | TyWild
  | TyNil










getImportDependencies :: [BzoFileData] -> [String]
getImportDependencies fs = nub $ concatMap (\(BzoFileData mn fp dm ast im ln ia la) -> im ++ (map snd ia)) fs










getLinkDependencies :: [BzoFileData] -> [String]
getLinkDependencies fs = nub $ concatMap (\(BzoFileData mn fp dm ast im ln ia la) -> ln ++ (map snd la)) fs
