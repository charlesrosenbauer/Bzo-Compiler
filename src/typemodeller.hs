module BzoTypeModeller where
import BzoSyntax
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










getImportDependencies :: BzoFileData -> [String]
getImportDependencies fs = nub $ (\(BzoFileData mn fp dm ast im ln ia la) -> im ++ (map fst ia)) fs










getLinkDependencies :: [BzoFileData] -> [String]
getLinkDependencies fs = nub $ concatMap (\(BzoFileData mn fp dm ast im ln ia la) -> ln ++ (map fst la)) fs










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










-- | Return (Function Names, Type Names, Fn Types)
getCallIds :: BzoSyntax -> ([(String, BzoSyntax)], [(String, BzoSyntax)], [(String, BzoSyntax)])
getCallIds (BzS_Calls     p      xs) = zip3Map $ map getCallIds xs
getCallIds (BzS_FnTypeDef p   f   d) = ([                            ], [                         ], [(f, BzS_FnTypeDef p   f   d)])
getCallIds (BzS_TypDef    p i t   d) = ([                            ], [(t, BzS_TypDef p i t   d)], [                            ])
getCallIds (BzS_FunDef    p i f o d) = ([(f, BzS_FunDef    p i f o d)], [                         ], [                            ])
getCallIds _                         = ([                            ], [                         ], [                            ])










reduceCallIds :: ([(String, BzoSyntax)], [(String, BzoSyntax)], [(String, BzoSyntax)]) -> Either [BzoErr] (Map String [BzoSyntax], Map String [BzoSyntax], Map String [BzoSyntax])
reduceCallIds (fns, tys, fts) =
  let fns'  = insertManyList Data.Map.Strict.empty fns
      tys'  = insertManyList Data.Map.Strict.empty tys
      fts'  = insertManyList Data.Map.Strict.empty fts
      fnset = keysSet fns'
      ftset = keysSet fts'
      errs  = if (isSubsetOf ftset fnset)
                then []
                else map (\nm -> DepErr $ nm ++ " has a type definition, but no function definition.") $ Prelude.filter (\a -> Data.Set.member a ftset) $ Data.Set.elems $ Data.Set.difference ftset fnset
  in case errs of
      []   -> Right (fns', tys', fts')
      errs -> Left errs










-- Function, Type, Container, Enum, Record
data ObjKind = FnObj | TyObj | CtObj | EnmObj | RcdObj










-- | Type for storing calls and definitions
data CallObj
  = FnCallObj  [BzoSyntax]
  | TyCallObj  [BzoSyntax]
  | CtCallObj  [BzoSyntax]
  | EnmCallObj  BzoSyntax
  | RcdCallObj  BzoSyntax









-- Different Symbol Tables will contain data for different steps
data SymbolTable
  = SymbolTable0 {
      st_module  :: String,
      st_domain  :: String,
      st_path    :: String,
      st_fndefs  :: Map String [BzoSyntax],
      st_ftdefs  :: Map String [BzoSyntax],
      st_tydefs  :: Map String [BzoSyntax],
      st_hints   :: [BzoSyntax],
      st_import  :: [String],
      st_importAs:: [(String, String)],
      st_link    :: [String],
      st_linkAs  :: [(String, String)] }
  | SymbolTable1 {
      st_module  :: String,
      st_domain  :: String,
      st_path    :: String,
      st_nmTable :: Map String Int,
      st_idTable :: Map Int CallObj,
      st_hints   :: [BzoSyntax],
      st_import  :: [String],
      st_importAs:: [(String, String)],
      st_link    :: [String],
      st_linkAs  :: [(String, String)] }










filedataToSymbolTable :: BzoFileData -> SymbolTable
filedataToSymbolTable (BzoFileData mn fp dm (BzS_Calls p ast) imp lnk is ls) =
  let (fndefs, else0) = Data.List.partition (matchSyntax MP_FunDef   ) ast
      (ftdefs, else1) = Data.List.partition (matchSyntax MP_FnTypeDef) else0
      (tydefs, hints) = Data.List.partition (matchSyntax MP_TypDef   ) else1
      fndefs'         = map (\xs -> (fnid $ head xs, xs)) $ groupBy (\a b -> (fnid a) == (fnid b)) fndefs
      ftdefs'         = map (\xs -> (fnid $ head xs, xs)) $ groupBy (\a b -> (fnid a) == (fnid b)) ftdefs
      tydefs'         = map (\xs -> (tyid $ head xs, xs)) $ groupBy (\a b -> (tyid a) == (tyid b)) tydefs
      fndefs''        = insertMany Data.Map.Strict.empty fndefs'
      ftdefs''        = insertMany Data.Map.Strict.empty ftdefs'
      tydefs''        = insertMany Data.Map.Strict.empty tydefs'
  in (SymbolTable0 mn dm fp fndefs'' ftdefs'' tydefs'' hints imp is lnk ls)








{-
getSymbolTable :: [BzoFileData] -> Either [BzoErr] (Map String (Map String SymbolTable))
getSymbolTable fs =
  let order = map (\x -> (bfd_moduleName x, bfd_domain x, x)) fs
      fileTable = doubleInsertMany order Data.Map.Strict.empty

  in Left []
-}
