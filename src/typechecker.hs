module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import GHC.Exts
import Data.List hiding (map, foldl, insert)
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










getImportDependencies :: BzoFileData -> [String]
getImportDependencies fs = nub $ (\(BzoFileData mn fp dm ast im ln ia la) -> im ++ (map snd ia)) fs










getLinkDependencies :: BzoFileData -> [String]
getLinkDependencies fs = nub $ (\(BzoFileData mn fp dm ast im ln ia la) -> ln ++ (map snd la)) fs










containsManyMembers :: Ord a => Map a b -> [a] -> Bool
containsManyMembers mp as = all (\x -> member x mp) as










orderByImports :: Map String BzoFileData -> [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderByImports mp [] = Right $ elems mp
orderByImports mp fs =
  let (next, remain) = break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = map (\x -> (bfd_domain x, x)) next
  in case next of
    [] -> Left [CfgErr "Circular Dependencies! Compilation cannot continue."]
    nx -> orderByImports (insertMany mp next') remain










--orderFileData :: [BzoFileData] -> [BzoFileData]
--orderFileData fs =
--  let f0 = groupWith bfd_domain fs
