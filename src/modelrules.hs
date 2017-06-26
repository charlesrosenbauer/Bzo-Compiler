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










fuseNameObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseNameObj (BzS_TyId p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExTypObj p0 i0 i1)]
fuseNameObj (BzS_Id   p0 i0) (BzS_Namespace p1 i1) = [(BzS_ExFunObj p0 i0 i1)]
fuseNameObj a                b                     = [a, b]










fuseFilterObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseFilterObj sn@(BzS_TyId  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Id    p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Cmpd  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Poly  p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_TyVar p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_Box   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj sn@(BzS_MId   p0 x) (BzS_Filter p1 f) = [(BzS_FilterObj p0 sn f)]
fuseFilterObj a                b                     = [a, b]










-- | Assumes that the syntax stream is reversed
fuseArrayObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseArrayObj sn@(BzS_TyId        p0 i) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_TyId        p0 i) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Id          p0 i) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Id          p0 i) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Cmpd        p0 x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Cmpd        p0 x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Poly        p0 x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Poly        p0 x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_TyVar       p0 x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_TyVar       p0 x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Box         p0 x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_Box         p0 x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_BTId        p0 x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_BTId        p0 x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ArrayObj  p0 o x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ArrayObj  p0 o x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_FilterObj p0 o x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_FilterObj p0 o x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ExTypObj  p0 o x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ExTypObj  p0 o x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ExFunObj  p0 o x) ar@(BzS_ArrGenMod p1  ) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj sn@(BzS_ExFunObj  p0 o x) ar@(BzS_ArrSzMod  p1 s) = [(BzS_ArrayObj p0 sn [ar])]
fuseArrayObj a                       b                       = [a, b]










fuseMapObj :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
fuseMapObj    (BzS_Box    p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0  x)]
fuseMapObj sn@(BzS_Id     p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_MId    p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Lambda p0 x d) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Cmpd   p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj sn@(BzS_Poly   p0 x  ) (BzS_MapMod p1) = [(BzS_MapObj p0 sn)]
fuseMapObj a                      b               = [a, b]










simplifyASTPass :: ([BzoSyntax] -> [BzoSyntax]) -> (BzoSyntax -> BzoSyntax -> [BzoSyntax]) -> BzoSyntax -> BzoSyntax
simplifyASTPass xf f (BzS_ArrayObj    p o x) = (BzS_ArrayObj   p (simplifyASTPass xf f o) (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_CurryObj    p o x) = (BzS_CurryObj   p (simplifyASTPass xf f o) (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_Calls         p x) = (BzS_Calls      p (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_Cmpd          p x) = (BzS_Cmpd       p (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_Poly          p x) = (BzS_Poly       p (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_Expr          p x) = (BzS_Expr       p (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_Block         p x) = (BzS_Block      p (map (simplifyASTPass xf f) (simplifyList (xf x) f)))
simplifyASTPass xf f (BzS_FilterObj   p x l) = (BzS_FilterObj  p (simplifyASTPass xf f x) (simplifyASTPass xf f l))
simplifyASTPass xf f (BzS_Lambda      p x d) = (BzS_Lambda     p (simplifyASTPass xf f x) (simplifyASTPass xf f d))
simplifyASTPass xf f (BzS_FnTy        p i o) = (BzS_FnTy       p (simplifyASTPass xf f i) (simplifyASTPass xf f o))
simplifyASTPass xf f (BzS_Box           p x) = (BzS_Box        p (simplifyASTPass xf f x))
simplifyASTPass xf f (BzS_Filter        p x) = (BzS_Filter     p (simplifyASTPass xf f x))
simplifyASTPass xf f (BzS_MapObj        p x) = (BzS_MapObj     p (simplifyASTPass xf f x))
simplifyASTPass xf f (BzS_ArrExprMod    p x) = (BzS_ArrExprMod p (simplifyASTPass xf f x))
simplifyASTPass xf f sn@(BzS_ArrGenMod    _) = sn
simplifyASTPass xf f sn@(BzS_ArrSzMod   _ _) = sn
simplifyASTPass xf f sn@(BzS_ExTypObj _ _ _) = sn
simplifyASTPass xf f sn@(BzS_ExFunObj _ _ _) = sn
simplifyASTPass xf f sn@(BzS_Flt        _ _) = sn
simplifyASTPass xf f sn@(BzS_Str        _ _) = sn
simplifyASTPass xf f sn@(BzS_Int        _ _) = sn
simplifyASTPass xf f sn@(BzS_Id         _ _) = sn
simplifyASTPass xf f sn@(BzS_BId        _ _) = sn
simplifyASTPass xf f sn@(BzS_BTId       _ _) = sn
simplifyASTPass xf f sn@(BzS_TyId       _ _) = sn
simplifyASTPass xf f sn@(BzS_TyVar      _ _) = sn
simplifyASTPass xf f sn@(BzS_MId        _ _) = sn
simplifyASTPass xf f sn@(BzS_Wildcard     _) = sn
simplifyASTPass xf f sn@(BzS_Nil          _) = sn
simplifyASTPass xf f sn@(BzS_Namespace  _ _) = sn
simplifyASTPass xf f sn@(BzS_MapMod       _) = sn
simplifyASTPass xf f sn@(BzS_Undefined     ) = sn










-- Add more passes, return errs if Namespace, Filter, Array, Map, etc. objects remain
simplifyAST :: BzoSyntax -> Either [BzoErr] BzoSyntax
simplifyAST ast =
  let pass0 = simplifyASTPass id fuseNameObj   ast
      pass1 = simplifyASTPass id fuseFilterObj pass0
      pass2 = simplifyASTPass reverse fuseArrayObj pass1
      pass3 = simplifyASTPass id fuseMapObj pass2
  in (Right pass1)










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
