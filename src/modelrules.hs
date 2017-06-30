module ModelRules where
import BzoSyntax
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe










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
      mta_filt:: Maybe ModelType }
  | MTA_Fun {
      mta_pos :: BzoPos,
      mta_id  :: String }
  | MTA_Type {
      mta_pos :: BzoPos,
      mta_id  :: String,
      mta_filt:: Maybe ModelType }
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










modelSimpleTypeParameter :: BzoSyntax -> Either [BzoErr] ModelType
modelSimpleTypeParameter (BzS_Expr p0 [(BzS_Cmpd  p1 xs)]) =
  let xs' = map modelSimpleTypeParameter xs
      es  = lefts  xs'
      ms  = rights xs'
  in case (es, ms) of
      ([], ty) -> Right (MT_Cmpd p0 Nothing ty)
      (er, ty) -> Left $ concat er
modelSimpleTypeParameter (BzS_Expr p0 [(BzS_TyVar p1 i)]) = Right (MT_TyAtom p0 Nothing (MTA_Var p1 i Nothing))
modelSimpleTypeParameter (BzS_Expr p0 [(BzS_TyVar p1 i), (BzS_Filter p2 flt)]) =
  case (modelType flt) of
    Left errs -> Left errs
    Right typ -> Right (MT_TyAtom p0 Nothing (MTA_Var p1 i (Just typ)))
modelSimpleTypeParameter x = Left [TypeErr (pos x) "Invalid Type Parameters"]










{-
  TODO:
    * Atom Modeller (tyvr)
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

modelType (BzS_Int   p i) = Right (MT_TyAtom p Nothing (MTA_Int  p i))
modelType (BzS_Str   p s) = Right (MT_TyAtom p Nothing (MTA_Str  p s))
modelType (BzS_Flt   p f) = Right (MT_TyAtom p Nothing (MTA_Flt  p f))
modelType (BzS_Nil   p  ) = Right (MT_TyAtom p Nothing (MTA_Nil  p  ))
modelType (BzS_Id    p i) = Right (MT_TyAtom p Nothing (MTA_Fun  p i))
modelType (BzS_BId   p i) = Right (MT_TyAtom p Nothing (MTA_Fun  p i))
modelType (BzS_TyId  p i) = Right (MT_TyAtom p Nothing (MTA_Type p i Nothing))
modelType (BzS_BTId  p i) = Right (MT_TyAtom p Nothing (MTA_Type p i Nothing))
