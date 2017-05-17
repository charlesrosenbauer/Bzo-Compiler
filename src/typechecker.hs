module BzoChecker where
import BzoSyntax
import BzoTypes
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.List hiding (map, foldl, insert)
import Data.Map.Strict hiding (map, foldl)
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
getImportDependencies fs = nub $ (\(BzoFileData mn fp dm ast im ln ia la) -> im ++ (map fst ia)) fs










getLinkDependencies :: [BzoFileData] -> [String]
getLinkDependencies fs = nub $ concatMap (\(BzoFileData mn fp dm ast im ln ia la) -> ln ++ (map fst la)) fs










containsManyMembers :: Ord a => Map a b -> [a] -> Bool
containsManyMembers mp as = all (\x -> member x mp) as










orderByImports :: Map String BzoFileData -> [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderByImports mp [] = Right $ elems mp
orderByImports mp fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = map (\x -> (bfd_moduleName x, x)) next
      domain         = if (bfd_domain (head fs) == "@")
                        then "Project Files"
                        else bfd_domain $ head fs
  in case next of
    [] -> Left [CfgErr ("Unsatisfiable Dependencies in " ++ domain ++ "! Compilation cannot continue.\n")]
    nx -> orderByImports (insertMany mp next') remain










orderByLinks :: Map String [BzoFileData] -> [[BzoFileData]] -> Either [BzoErr] [[BzoFileData]]
orderByLinks mp [] = Right $ elems mp
orderByLinks mp fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getLinkDependencies x) fs
      next'          = map (\x -> (bfd_domain $ head x, x)) next
  in case next of
    [] -> Left [CfgErr "Unsatisfiable Dependencies between libraries! Compilation cannot continue.\n"]
    nx -> orderByLinks (insertMany mp next') remain









orderFileData :: [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderFileData fs =
  let f0 = groupWith bfd_domain fs
      f1 = map (orderByImports empty) f0
      f2 = concat $ lefts  f1
      f3 = [orderByLinks empty $ rights f1]
      f4 = lefts  f3
      f5 = concat $ rights f3
  in case (f2, f4) of
      ([], []) -> Right $ concat f5
      ([], er) -> Left  $ concat er
      (er, _ ) -> Left  er
