module ModelRules where
import BzoSyntax
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe










data TypeAST
      = TA_Cmpd {
          ta_pos :: BzoPos,
          ta_exs :: [TypeAST] }
      | TA_Poly {
          ta_pos :: BzoPos,
          ta_exs :: [TypeAST] }
      | TA_Expr {
          ta_pos :: BzoPos,
          ta_exp :: TypeAST }
      | TA_Filt {
          ta_pos :: BzoPos,
          ta_filt :: TypeAST,
          ta_exp  :: TypeAST }
      | TA_FnTy {
          ta_pos :: BzoPos,
          ta_in   :: TypeAST,
          ta_out  :: TypeAST }
      | TA_Enum {
          ta_pos :: BzoPos,
          ta_id   :: String,
          ta_exp  :: TypeAST }
      | TA_Record {
          ta_pos :: BzoPos,
          ta_id   :: String,
          ta_exp  :: TypeAST }
      | TA_Curry {
          ta_pos :: BzoPos,
          ta_crs  :: [TypeAST],
          ta_exp  :: TypeAST }
      | TA_Arr {
          ta_pos :: BzoPos,
          ta_szs  :: [Int],
          ta_exp  :: TypeAST }
      | TA_IntLit {
          ta_pos :: BzoPos,
          ta_int  :: Integer }
      | TA_FltLit {
          ta_pos :: BzoPos,
          ta_flt  :: Double }
      | TA_StrLit {
          ta_pos :: BzoPos,
          ta_str  :: String }
      | TA_FnLit  {
          ta_pos :: BzoPos,
          ta_fn   :: String }
      | TA_TyLit  {
          ta_pos :: BzoPos,
          ta_ty   :: String }
      | TA_BFnLit {
          ta_pos :: BzoPos,
          ta_fn   :: String }
      | TA_BTyLit {
          ta_pos :: BzoPos,
          ta_ty   :: String }
      | TA_Nil{
          ta_pos :: BzoPos }










data ModelRecord = ModelRecord{
    mr_pos    :: BzoPos,
    mr_name   :: String,
    mr_parent :: String,
    mr_type   :: TypeAST }










data ModelEnum = ModelEnum{
    me_pos    :: BzoPos,
    me_name   :: String,
    me_parent :: String,
    me_type   :: TypeAST }










-- | Basic Type Expression. No Records or Enums. Used for filters, etc.
modelBasicType :: BzoSyntax -> Either [BzoErr] TypeAST
modelBasicType (BzS_Int  p i)  = Right (TA_IntLit p i)
modelBasicType (BzS_Flt  p f)  = Right (TA_FltLit p f)
modelBasicType (BzS_Str  p s)  = Right (TA_StrLit p s)
modelBasicType (BzS_Id   p x)  = Right (TA_FnLit  p x)
modelBasicType (BzS_TyId p x)  = Right (TA_TyLit  p x)
modelBasicType (BzS_BId  p x)  = Right (TA_BFnLit p x)
modelBasicType (BzS_BTId p x)  = Right (TA_BTyLit p x)
modelBasicType (BzS_Nil  p  )  = Right (TA_Nil    p  )

modelBasicType (BzS_FnTy p i e) =
  let i'  = [modelBasicType i]
      e'  = [modelBasicType e]
      ers = (lefts  i') ++ (lefts e')
      vli = head $ rights i'
      vle = head $ rights e'
  in case ers of
      [] -> Right (TA_FnTy p vli vle)
      er -> Left $ concat er

modelBasicType (BzS_Cmpd p xs) =
  let xs' = [map modelBasicType xs]
      ers = concatMap lefts  xs'
      vls = concatMap rights xs'
  in case ers of
      [] -> Right (TA_Cmpd p vls)
      er -> Left $ concat er

modelBasicType (BzS_Poly p xs) =
  let xs' = [map modelBasicType xs]
      ers = concatMap lefts  xs'
      vls = concatMap rights xs'
  in case ers of
      [] -> Right (TA_Poly p vls)
      er -> Left $ concat er

modelBasicType (BzS_Box p x) =
  let xs' = [[modelBasicType x]]
      ers = concatMap lefts  xs'
      vls = concatMap rights xs'
  in case ers of
      [] -> Right $ head vls
      er -> Left $ concat er
