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










orderByImports :: Map String BzoFileData -> [BzoFileData] -> [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderByImports mp out [] = Right out
orderByImports mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = map (\x -> (bfd_moduleName x, x)) next
      domain         = if (bfd_domain (head fs) == "@")
                        then "Project Files"
                        else bfd_domain $ head fs
  in case next of
    [] -> Left [CfgErr ("Unsatisfiable Dependencies in " ++ domain ++ "! Compilation cannot continue.\n")]
    nx -> orderByImports (insertMany mp next') (out ++ next) remain










orderByLinks :: Map String [BzoFileData] -> [[BzoFileData]] -> [[BzoFileData]] -> Either [BzoErr] [[BzoFileData]]
orderByLinks mp out [] = Right out
orderByLinks mp out fs =
  let (remain, next) = break (\x -> containsManyMembers mp $ getLinkDependencies x) fs
      next'          = map (\x -> (bfd_domain $ head x, x)) next
  in case next of
    [] -> Left [CfgErr "Unsatisfiable Dependencies between libraries! Compilation cannot continue.\n"]
    nx -> orderByLinks (insertMany mp next') (out ++ next) remain









orderFileData :: [BzoFileData] -> Either [BzoErr] [BzoFileData]
orderFileData fs =
  let f0 = groupWith bfd_domain $ map appendStdDep fs
      f1 = map (orderByImports empty []) f0
      f2 = concat $ lefts  f1
      f3 = [orderByLinks empty [] $ rights f1]
      f4 = lefts  f3
      f5 = concat $ rights f3
  in case (f2, f4) of
      ([], []) -> Right $ concat f5
      ([], er) -> Left  $ concat er
      (er, _ ) -> Left  er










appendStdDep :: BzoFileData -> BzoFileData
appendStdDep (BzoFileData mn fp "Std" ast imp lnk ima lna) = (BzoFileData mn fp "Std" ast imp lnk ima lna)
appendStdDep (BzoFileData mn fp dmn   ast imp lnk ima lna) =
  case (elem "Std" imp, elem "Std" $ map fst ima) of
    (False, False) -> (BzoFileData mn fp dmn ast imp (lnk ++ ["Std"]) ima lna)
    (_    , _    ) -> (BzoFileData mn fp dmn ast imp lnk ima lna)










zip3Map :: [([a], [b], [c])] -> ([a], [b], [c])
zip3Map = ((\(x, y, z) -> (concat x, concat y, concat z)) . unzip3)










-- | Returns (Ids/BI Ids, TyIds/BI TyIds, MIds)
getIds :: BzoSyntax -> ([String], [String], [String])
getIds (BzS_Expr       p xs) = zip3Map $ map getIds xs
getIds (BzS_Block      p xs) = zip3Map $ map getIds xs
getIds (BzS_MId        p st) = ([  ], [  ], [st])
getIds (BzS_TyId       p st) = ([  ], [st], [  ])
getIds (BzS_Id         p st) = ([st], [  ], [  ])
getIds (BzS_BId        p st) = ([st], [  ], [  ])
getIds (BzS_BTId       p st) = ([  ], [st], [  ])
getIds (BzS_Filter     p xs) = getIds xs
getIds (BzS_Box        p xs) = getIds xs
getIds (BzS_ArrExprMod p xs) = getIds xs
getIds (BzS_Cmpd       p xs) = zip3Map $ map getIds xs
getIds (BzS_Poly       p xs) = zip3Map $ map getIds xs
getIds (BzS_FnTy    p as bs) = zip3Map [getIds as, getIds bs]
getIds (BzS_Calls      p xs) = zip3Map $ map getIds xs

getIds (BzS_FunDef     p i f o d) = zip3Map [getIds i, getIds o, getIds d, ([f], [ ], [ ])]
getIds (BzS_FnTypeDef  p   f   d) = zip3Map [                    getIds d, ([f], [ ], [ ])]
getIds (BzS_TypDef     p i t   d) = zip3Map [getIds i,           getIds d, ([ ], [t], [ ])]

getIds _                     = ([  ], [  ], [  ])
