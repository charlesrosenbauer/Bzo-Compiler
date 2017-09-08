module BzoFileSorter where
import BzoTypes
import BzoParser
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.Maybe
import Data.List hiding (map, foldl, foldr, insert)
import Data.Map.Strict hiding (map, foldl, foldr, mapEither)
import Data.Set hiding (map, foldl, foldr, mapEither, empty)
import HigherOrder
import ModelRules
import Debug.Trace










getImportDependencies :: BzoFileModel -> [String]
getImportDependencies fs = nub $ (\(BzoFileModel mn fp dm ast im ln ia la) -> im ++ (map fst ia)) fs










getLinkDependencies :: [BzoFileModel] -> [String]
getLinkDependencies fs = nub $ concatMap (\(BzoFileModel mn fp dm ast im ln ia la) -> ln ++ (map fst la)) fs










orderByImports :: Map String BzoFileModel -> [BzoFileModel] -> [BzoFileModel] -> Either [BzoErr] [BzoFileModel]
orderByImports mp out [] = Right out
orderByImports mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = map (\x -> (bfm_moduleName x, x)) next
      domain         = if (bfm_domain (head fs) == "@")
                        then "Project Files"
                        else bfm_domain $ head fs
  in case next of
    [] -> Left [CfgErr ("Unsatisfiable Dependencies in " ++ domain ++ "! Compilation cannot continue.\n")]
    nx -> orderByImports (insertMany mp next') (out ++ next) remain










orderByLinks :: Map String [BzoFileModel] -> [[BzoFileModel]] -> [[BzoFileModel]] -> Either [BzoErr] [[BzoFileModel]]
orderByLinks mp out [] = Right out
orderByLinks mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getLinkDependencies x) fs
      next'          = map (\x -> (bfm_domain $ head x, x)) next
  in case next of
    [] -> Left [CfgErr "Unsatisfiable Dependencies between libraries! Compilation cannot continue.\n"]
    nx -> orderByLinks (insertMany mp next') (out ++ next) remain









orderFileData :: [BzoFileModel] -> Either [BzoErr] [BzoFileModel]
orderFileData fs =
  let f0 = groupWith bfm_domain $ map appendStdDep fs
      f1 = map (orderByImports empty []) f0
      f2 = concat $ lefts  f1
      f3 = [orderByLinks empty [] $ rights f1]
      f4 = lefts  f3
      f5 = concat $ rights f3
  in case (f2, f4) of
      ([], []) -> Right $ concat f5
      ([], er) -> Left  $ concat er
      (er, _ ) -> Left  er










appendStdDep :: BzoFileModel -> BzoFileModel
appendStdDep (BzoFileModel mn fp "Std" ast imp lnk ima lna) = (BzoFileModel mn fp "Std" ast imp lnk ima lna)
appendStdDep (BzoFileModel mn fp dmn   ast imp lnk ima lna) =
  case (elem "Std" imp, elem "Std" $ map fst ima) of
    (False, False) -> (BzoFileModel mn fp dmn ast imp (lnk ++ ["Std"]) ima lna)
    (_    , _    ) -> (BzoFileModel mn fp dmn ast imp lnk ima lna)
