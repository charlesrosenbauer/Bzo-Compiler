module BzoTypeModeller where
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










data FuncModel
  = FuncExpr {
      funcName  :: String }
  | FuncAlias {
      funcName  :: String,
      funcAlias :: String }
  | FuncConstant {
      funcName  :: String }
  | FuncGraph {
      funcName  :: String }










data TypeModel
  = TM_CMPD {
      tm_exprs :: [TypeModel] }
  | TM_POLY {
      tm_exprs :: [TypeModel] }
  | TM_ATOM {
      tm_atom  :: String }  -- Change this later
  | TM_ENUM {
      tm_enumName :: String,
      tm_enumType :: TypeModel }
  | TM_RECORD {
      tm_recordName :: String,
      tm_recordType :: TypeModel }










data TypeDefModel
  = TypeStruct {
      typeName  :: String,
      typeDomain:: String,
      typepos   :: BzoPos,
      typepars  :: [String],
      typedef   :: TypeModel,
      typerecs  :: [(String, TypeModel)],
      typeenum  :: [(String, TypeModel)]}
  | TypeContainer {
      typeName  :: String,
      typeDomain:: String,
      typepos   :: BzoPos,
      typepars  :: [String],
      typedef   :: TypeModel,
      typerecs  :: [(String, TypeModel)],
      typeenum  :: [(String, TypeModel)]}
  | TypeAlias {
      typeName  :: String,
      typeDomain:: String,
      typepos   :: BzoPos,
      typeAlias :: String }
  | TypeAliasContainer {
      typeName  :: String,
      typeDomain:: String,
      typepos   :: BzoPos,
      typepars  :: [String],
      typeAlias :: String }










data BzoFileTypeData
  = BzoFileTypeData {
      bft_module    :: String,
      bft_domain    :: String,
      bft_ast       :: BzoSyntax,
      bft_functions :: [(String, [BzoSyntax])],
      bft_typedefs  :: [(String, [BzoSyntax])],
      bft_fntypes   :: [(String, [BzoSyntax])],
      bft_impfuncs  :: [String],
      bft_imptypes  :: [String],
      bft_impftypes :: [String] }










data BzoRecord
  = BzoRecord {
      br_pos :: BzoPos,
      br_nm  :: String,
      br_ty  :: BzoSyntax }










data BzoEnum
  = BzoEnum {
      be_pos :: BzoPos,
      be_nm  :: String,
      be_ty  :: BzoSyntax }








isValidBId :: String -> Bool
isValidBId "$add-binop"    = True
isValidBId "$sub-binop"    = True
isValidBId "$mul-binop"    = True
isValidBId "$div-binop"    = True
isValidBId "$mod-binop"    = True
isValidBId "$gtr-binop"    = True
isValidBId "$lss-binop"    = True
isValidBId "$geq-binop"    = True
isValidBId "$leq-binop"    = True
isValidBId "$eql-binop"    = True
isValidBId "$neq-binop"    = True
isValidBId "$arrLength"    = True
isValidBId "$getIndex"     = True
isValidBId "$setIndex"     = True
isValidBId "$map"          = True
isValidBId "$fold"         = True
isValidBId "$reduce"       = True
isValidBId "$scam"         = True
isValidBId "$chain"        = True
isValidBId "$zip"          = True
isValidBId "$exp-binop"    = True
isValidBId "$surd-binop"   = True
isValidBId "$log-binop"    = True
isValidBId "$sine"         = True
isValidBId "$cosine"       = True
isValidBId "$tangent"      = True
isValidBId "$arcsine"      = True
isValidBId "$arccosine"    = True
isValidBId "$arctangent"   = True
isValidBId "$hsine"        = True
isValidBId "$hcosine"      = True
isValidBId "$htangent"     = True
isValidBId "$arc-hsine"    = True
isValidBId "$arc-hcosine"  = True
isValidBId "$arc-htangent" = True
isValidBId "$floor"        = True
isValidBId "$ciel"         = True
isValidBId "$round"        = True
isValidBId "$factorial"    = True
isValidBId "$gamma"        = True
isValidBId "$nCr"          = True
isValidBId "$nPr"          = True
isValidBId "$sqrt"         = True
isValidBId "$cbrt"         = True
isValidBId "$log2"         = True
isValidBId "$log10"        = True
isValidBId "$to-uppercase" = True
isValidBId "$to-lowercase" = True
isValidBId "$isPrefix"     = True
isValidBId "$isSuffix"     = True
isValidBId "$append-array" = True
isValidBId "$print"        = True
isValidBId "and-binop"     = True
isValidBId "$or-binop"     = True
isValidBId "$xor-binop"    = True
isValidBId "$not-op"       = True
isValidBId "$rshift-binop" = True
isValidBId "$lshift-binop" = True
isValidBId "$ctlz-op"      = True
isValidBId "$cttz-op"      = True
isValidBId "$popcount-op"  = True
isValidBId "$toInt"        = True
isValidBId "$toUnt"        = True
isValidBId "$toUnm"        = True
isValidBId "$toFlt"        = True
isValidBId "$toBits"       = True
isValidBId _               = False










isValidBTId :: String -> Bool
isValidBTId "$Int8"      = True
isValidBTId "$Int16"     = True
isValidBTId "$Int32"     = True
isValidBTId "$Int64"     = True
isValidBTId "$Unt8"      = True
isValidBTId "$Unt16"     = True
isValidBTId "$Unt32"     = True
isValidBTId "$Unt64"     = True
isValidBTId "$Flt16"     = True
isValidBTId "$Flt32"     = True
isValidBTId "$Flt64"     = True
isValidBTId "$Unm16"     = True
isValidBTId "$Unm32"     = True
isValidBTId "$Unm64"     = True
isValidBTId "$BoolTF"    = True
isValidBTId "$BoolFT"    = True
isValidBTId "$Char"      = True
isValidBTId "$ASCII"     = True
isValidBTId "$Unique"    = True
isValidBTId "$Effect"    = True
isValidBTId "$Byte"      = True
isValidBTId "$Half"      = True
isValidBTId "$Word"      = True
isValidBTId "$Dword"     = True
isValidBTId _            = False










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










matchRecord :: BzoSyntax -> Maybe BzoRecord
matchRecord (BzS_Expr p [(BzS_Id _ i), (BzS_Filter _ t)]) = Just (BzoRecord p i t)
matchRecord _                                             = Nothing










matchEnum :: BzoSyntax -> Maybe BzoEnum
matchEnum (BzS_Expr p [(BzS_TyId _ i), (BzS_Filter _ t)]) = Just (BzoEnum p i t)
matchEnum _                                               = Nothing








-- Temporary. Will need to figure out how to actually handle records and enums.
--extractRecordData :: BzoSyntax -> Maybe (BzoPos, String, BzoSyntax)
--extractRecordData ((BzS_Id p0 i) : (BzS_Filter p1 t) : xs) = Just (p0, i, t)
--extractRecordData _ = Nothing









--extractRecordTypes :: BzoSyntax -> [(String, BzoSyntax)]
--extractRecordTypes (BzS_Cmpd ps exprs) =
