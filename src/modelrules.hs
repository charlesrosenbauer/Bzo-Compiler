module ModelRules where
import BzoSyntax
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe










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
      tm_atom  :: TypeAtom }  -- Change this later
  | TM_ENUM {
      tm_enumName :: String,
      tm_enumType :: TypeModel }
  | TM_RECORD {
      tm_recordName :: String,
      tm_recordType :: TypeModel }










data TypeAtom
  = TA_Int {
      ta_int :: Integer }
  | TA_Flt {
      ta_flt :: Double }
  | TA_Str {
      ta_str :: String }
  | TA_Var {
      ta_id  :: String,
      ta_filt:: TypeModel }









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
      typepars  :: [TypeAtom],
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









{-
modelType :: String -> BzoSyntax -> Either [BzoErr] TypeDefModel
modelType dm (BzS_TypDef ps BzS_Undefined tid (BzS_Expr p [(BzS_BTId _ b)])) =
  if(isValidBTId b)
    then $ Right (TypeAlias tid dm ps b)
    else $ Left  [ModelErr p (b ++ " is not a recognized built-in.")]
modelType dm (BzS_TypDef ps pars tid (BzS_Expr p [(BzS_BTId _ b)])) =
  case (isValidBTId b, pars) of   -- Add function to check pars for validity
    (True , Right ps') -> Right  (TypeAliasContainer tid dm ps pars b)
    (False, Right ps') -> Left   [ModelErr p (b ++ " is not a recognized built-in.")]
    (False, Left errs) -> Left $ [ModelErr p (b ++ " is not a recognized built-in.")] ++ errs
-}
