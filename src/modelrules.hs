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










data ModelTypeAtom
  = MTA_Int {
      mta_pos :: BzoPos,
      mta_int :: Integer }
  | MTA_Flt {
      mta_pos :: BzoPos,
      mta_flt :: Double }
  | MTA_Str {
      mta_pos :: BzoPos,
      mta_str :: String }
  | MTA_Var {
      mta_pos :: BzoPos,
      mta_id  :: String,
      mta_filt:: ModelType }
  | MTA_Fun {
      mta_pos :: BzoPos,
      mta_id  :: String }
  | MTA_Nil {
      mta_pos :: BzoPos }










data ModelArrayType
  = ModelTypeGenArr {
      mat_pos  :: BzoPos }
  | ModelTypeIntArr {
      mat_pos  :: BzoPos,
      mat_size :: Integer }










data ModelFilterTy = Maybe ModelType










data ModelType
  = MT_Cmpd {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_exp :: [ModelType] }
  | MT_Poly {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_exp :: [ModelType] }
  | MT_Enum {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_exp :: [ModelType],
      mt_enm :: [ModelEnum] }
  | MT_Record {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_exp :: [ModelType],
      mt_rcd :: [ModelRecord] }
  | MT_TyExpr {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_exp :: [ModelType] }
  | MT_TyAtom {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_atm :: ModelTypeAtom }
  | MT_FnTy {
      mt_pos :: BzoPos,
      mt_arr :: Maybe [ModelArrayType],
      mt_in  :: ModelType,
      mt_out :: ModelType }










data ModelRecord = ModelRecord{
      mr_pos  :: BzoPos,
      mr_name :: String,
      mr_type :: ModelType }










data ModelEnum = ModelEnum{
    me_pos  :: BzoPos,
    me_name :: String,
    me_type :: ModelType }










modelRecord :: BzoSyntax -> Either [BzoErr] ModelRecord
modelRecord (BzS_Expr p [(BzS_Id _ i), (BzS_Filter _ t)]) =
  case (modelType t) of
    Left errs -> Left  errs
    Right typ -> Right (ModelRecord p i typ)
modelRecord syn = Left [(ModelErr (pos syn) "Invalid Record")]










modelEnum :: BzoSyntax -> Either [BzoErr] ModelEnum
modelEnum (BzS_Expr p [(BzS_TyId _ i), (BzS_Filter _ t)]) =
  case (modelType t) of
    Left errs -> Left  errs
    Right typ -> Right (ModelEnum p i typ)
modelEnum syn = Left [(ModelErr (pos syn) "Invalid Enum")]










{-
  TODO:
    * Atom Modeller (fnid, tyid, tyvr, int, flt, str, nil)
    * Array Type Modeller (general, integer, list)
    * Type Expression Modeller
-}
modelType :: BzoSyntax -> Either [BzoErr] ModelType
modelType (BzS_Cmpd p xs) =
  let xs' = map (eitherFirst modelRecord modelType) xs
      ers = aChoices xs'
      rcs = bChoices xs'
      tps = cChoices xs'
  in case (ers, rcs, tps) of
        ([], [], ts) -> Right (MT_Cmpd   p Nothing ts)
        ([], rs, ts) -> Right (MT_Record p Nothing ts rs)
        (er, _ , _ ) -> Left  $ concat er

modelType (BzS_Poly p xs) =
  let xs' = map (eitherFirst modelEnum modelType) xs
      ers = aChoices xs'
      ens = bChoices xs'
      tps = cChoices xs'
  in case (ers, ens, tps) of
        ([], [], ts) -> Right (MT_Poly p Nothing ts)
        ([], es, ts) -> Right (MT_Enum p Nothing ts es)
        (er, _ , _ ) -> Left  $ concat er

modelType (BzS_FnTy p i e) =
  let x   = modelType i
      y   = modelType e
      ers = lefts  ([x] ++ [y])
      tx  = rights $ [x]
      ty  = rights $ [y]
  in case (ers, tx, ty) of
      ([], [i'], [e']) -> Right (MT_FnTy p Nothing i' e')
      (er, _   , _   ) -> Left  $ concat er

modelType (BzS_Int p i) = Right (MT_TyAtom p Nothing (MTA_Int p i))
modelType (BzS_Str p s) = Right (MT_TyAtom p Nothing (MTA_Str p s))
modelType (BzS_Flt p f) = Right (MT_TyAtom p Nothing (MTA_Flt p f))
modelType (BzS_Nil p  ) = Right (MT_TyAtom p Nothing (MTA_Nil p  ))
