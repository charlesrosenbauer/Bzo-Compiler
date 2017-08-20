module ModelRules where
import BzoSyntax
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe










data TypeAST
      = TA_Cmpd {
          ta_pos :: !BzoPos,
          ta_exs :: ![TypeAST] }
      | TA_Poly {
          ta_pos :: !BzoPos,
          ta_exs :: ![TypeAST] }
      | TA_Expr {
          ta_pos :: !BzoPos,
          ta_exp :: !TypeAST,
          ta_nxt :: !TypeAST }
      | TA_Filt {
          ta_pos :: !BzoPos,
          ta_filt :: !TypeAST,
          ta_exp :: !TypeAST }
      | TA_FnTy {
          ta_pos :: !BzoPos,
          ta_in  :: !TypeAST,
          ta_out :: !TypeAST }
      | TA_Enum {
          ta_pos :: !BzoPos,
          ta_id  :: !String,
          ta_exp :: !TypeAST }
      | TA_Record {
          ta_pos :: !BzoPos,
          ta_id  :: !String,
          ta_exp :: !TypeAST }
      | TA_Curry {
          ta_pos :: !BzoPos,
          ta_crs :: ![TypeAST],
          ta_exp :: !TypeAST }
      | TA_Arr {
          ta_pos :: !BzoPos,
          ta_szs :: ![Integer], -- | Size of Zero is General Array
          ta_exp :: !TypeAST }
      | TA_IntLit {
          ta_pos :: !BzoPos,
          ta_int :: !Integer }
      | TA_FltLit {
          ta_pos :: !BzoPos,
          ta_flt  :: !Double }
      | TA_StrLit {
          ta_pos :: !BzoPos,
          ta_str :: !String }
      | TA_FnLit  {
          ta_pos :: !BzoPos,
          ta_fn  :: !String }
      | TA_TyLit  {
          ta_pos :: !BzoPos,
          ta_ty  :: !String }
      | TA_ExFnLit  {
          ta_pos :: !BzoPos,
          ta_fn  :: !String,
          ta_loc :: !String }
      | TA_ExTyLit  {
          ta_pos :: !BzoPos,
          ta_ty  :: !String,
          ta_loc :: !String }
      | TA_BFnLit {
          ta_pos :: !BzoPos,
          ta_fn  :: !String }
      | TA_BTyLit {
          ta_pos :: !BzoPos,
          ta_ty  :: !String }
      | TA_TyVar {
          ta_pos :: !BzoPos,
          ta_id  :: !String }
      | TA_Nil{
          ta_pos :: !BzoPos }










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










-- ! Add More Cases for other calls
data CallAST
    = CA_TyDefCall {
        ca_pos     :: BzoPos,
        ca_id      :: String,
        ca_pars    :: [TypeAST],
        ca_records :: [ModelRecord],
        ca_enums   :: [ModelEnum],
        ca_tydef   :: TypeAST }
    | CA_FTDefCall {
        ca_pos     :: BzoPos,
        ca_id      :: String,
        ca_intype  :: TypeAST,
        ca_extype  :: TypeAST }










checkRecord :: BzoSyntax -> Maybe (BzoPos, String, BzoSyntax)
checkRecord (BzS_FilterObj p0 (BzS_Id  p1 i) ty) = Just (p0, i, ty)
checkRecord (BzS_FilterObj p0 (BzS_BId p1 i) ty) = Just (p0, i, ty)
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 (BzS_Id  p1 i) ty)]) = Just (p0, i, ty)
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BId p1 i) ty)]) = Just (p0, i, ty)
checkRecord _                                    = Nothing










checkEnum :: BzoSyntax -> Maybe (BzoPos, String, BzoSyntax)
checkEnum (BzS_FilterObj p0 (BzS_TyId p1 i) ty) = Just (p0, i, ty)
checkEnum (BzS_FilterObj p0 (BzS_BTId p1 i) ty) = Just (p0, i, ty)
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 (BzS_TyId p1 i) ty)]) = Just (p0, i, ty)
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BTId p1 i) ty)]) = Just (p0, i, ty)
checkEnum _                                     = Nothing










separateRecords :: BzoSyntax -> Either BzoSyntax (String, BzoSyntax)
separateRecords (BzS_Expr _ [(BzS_FilterObj p0 (BzS_Id  p1 i) ty)]) = Right (i, ty)
separateRecords (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BId p1 i) ty)]) = Right (i, ty)
separateRecords x                                    = Left  x










separateEnums :: BzoSyntax -> Either BzoSyntax (String, BzoSyntax)
separateEnums (BzS_Expr _ [(BzS_FilterObj p0 (BzS_TyId p1 i) ty)]) = Right (i, ty)
separateEnums (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BTId p1 i) ty)]) = Right (i, ty)
separateEnums x                                     = Left  x










getCompoundContents :: Either BzoSyntax (String, BzoSyntax) -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
getCompoundContents (Left       syn ) = modelType syn
getCompoundContents (Right (str, syn)) =
  let x  = [modelType syn]
      ls = lefts  x
      (rs, mrs, mes) = head $ rights x
  in case ls of
      []  -> Right ((TA_Record (pos syn) str rs), mrs, mes)
      ers -> Left $ concat ers










getPolymorphContents :: Either BzoSyntax (String, BzoSyntax) -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
getPolymorphContents (Left        syn ) = modelType syn
getPolymorphContents (Right (str, syn)) =
  let x  = [modelType syn]
      ls = lefts  x
      (es, mrs, mes) = head $ rights x
  in case ls of
      []  -> Right ((TA_Enum (pos syn) str es), mrs, mes)
      ers -> Left $ concat ers










toRecordModel :: String -> (BzoPos, String, TypeAST) -> ModelRecord
toRecordModel r (p, i, t) = (ModelRecord p i r t)










toEnumModel ::  String -> (BzoPos, String, TypeAST) -> ModelEnum
toEnumModel r (p, i, t) = (ModelEnum p i r t)










modelArrayObj :: BzoSyntax -> Either [BzoErr] Integer
modelArrayObj (BzS_ArrGnObj p  ) = Right 0
modelArrayObj (BzS_ArrSzObj p s) = Right s
modelArrayObj s                  = Left [(SntxErr (pos s) "No idea what happened here. Something's not right in the Array Syntax.")]










-- | Basic Type Expression. No Records or Enums. Used for filters, etc.
modelBasicType :: BzoSyntax -> Either [BzoErr] TypeAST
modelBasicType (BzS_Int   p i)  = Right (TA_IntLit p i)
modelBasicType (BzS_Flt   p f)  = Right (TA_FltLit p f)
modelBasicType (BzS_Str   p s)  = Right (TA_StrLit p s)
modelBasicType (BzS_Id    p x)  = Right (TA_FnLit  p x)
modelBasicType (BzS_TyId  p x)  = Right (TA_TyLit  p x)
modelBasicType (BzS_BId   p x)  = Right (TA_BFnLit p x)
modelBasicType (BzS_BTId  p x)  = Right (TA_BTyLit p x)
modelBasicType (BzS_Nil   p  )  = Right (TA_Nil    p  )
modelBasicType (BzS_TyVar p x)  = Right (TA_TyVar  p x)
modelBasicType (BzS_ExFunObj p x n) = Right (TA_ExFnLit  p x n)
modelBasicType (BzS_ExTypObj p x n) = Right (TA_ExTyLit  p x n)

modelBasicType (BzS_FnTy p i e) =
  let !i'  = [modelBasicType i]
      !e'  = [modelBasicType e]
      ers  = (lefts  i') ++ (lefts e')
      vli  = head $ rights i'
      vle  = head $ rights e'
  in case ers of
      [] -> Right (TA_FnTy p vli vle)
      er -> Left $ concat er

modelBasicType (BzS_Cmpd p xs) =
  let !xs' = [map modelBasicType xs]
      rcs  = catMaybes $ map checkRecord xs
      ers  = concatMap lefts  xs'
      vls  = concatMap rights xs'
  in case (ers, rcs) of
      ([], []) -> Right (TA_Cmpd p vls)
      (er, rs) -> Left $ (concat er) ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Record Syntax: " ++ n ++ "\n")) rs)

modelBasicType (BzS_Poly p xs) =
  let !xs' = [map modelBasicType xs]
      ens  = catMaybes $ map checkEnum xs
      ers  = concatMap lefts  xs'
      vls  = concatMap rights xs'
  in case (ers, ens) of
      ([], []) -> Right (TA_Poly p vls)
      (er, es) -> Left $ concat er ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Enum Syntax: " ++ n ++ "\n")) es)

modelBasicType (BzS_Box p x) = modelBasicType x

modelBasicType (BzS_Expr p [x]) = modelBasicType x

modelBasicType (BzS_FilterObj p o f) =
  let !o'  = [modelBasicType o]
      !f'  = [modelBasicType f]
      ers  = (lefts o') ++ (lefts f')
      vlo  = head $ rights o'
      vlf  = head $ rights f'
  in case ers of
      [] -> Right (TA_Filt p vlf vlo)
      er -> Left $ concat er

modelBasicType (BzS_MapObj p o) = Left [SntxErr p "Unexpected Map Syntax in Type Expression"]

modelBasicType (BzS_ArrayObj p o a) =
  let !o'  = [modelBasicType o]
      ers  = lefts  o'
      vls  = head $ rights o'
      szs  = map modelArrayObj a
      szx  = rights szs
      sze  = lefts szs
  in case (ers ++ sze) of
      [] -> Right $ (TA_Arr p szx vls)
      er -> Left  $ concat er

modelBasicType (BzS_Expr p (x:xs)) =
  let !x'  = [modelBasicType x]
      !xs' = [modelBasicType (BzS_Expr (pos $ head xs) xs)]
      ers  = (lefts x') ++ (lefts xs')
      vlx  = head $ rights x'
      vly  = head $ rights xs'
  in case (ers) of
      [] -> Right (TA_Expr p vlx vly)
      er -> Left $ concat er

modelBasicType (BzS_CurryObj p o x) = Left [SntxErr p "Currying in type expressions is currently not permitted."]

modelBasicType s = Left [SntxErr (pos s) "Unexpected Component of Type Expression."]










modelType :: BzoSyntax -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
modelType (BzS_Int   p i)  = Right ((TA_IntLit p i), [], [])
modelType (BzS_Flt   p f)  = Right ((TA_FltLit p f), [], [])
modelType (BzS_Str   p s)  = Right ((TA_StrLit p s), [], [])
modelType (BzS_Id    p x)  = Right ((TA_FnLit  p x), [], [])
modelType (BzS_TyId  p x)  = Right ((TA_TyLit  p x), [], [])
modelType (BzS_BId   p x)  = Right ((TA_BFnLit p x), [], [])
modelType (BzS_BTId  p x)  = Right ((TA_BTyLit p x), [], [])
modelType (BzS_Nil   p  )  = Right ((TA_Nil    p  ), [], [])
modelType (BzS_TyVar p x)  = Right ((TA_TyVar  p x), [], [])
modelType (BzS_ExFunObj p x n) = Right ((TA_ExFnLit  p x n), [], [])
modelType (BzS_ExTypObj p x n) = Right ((TA_ExTyLit  p x n), [], [])

modelType (BzS_FnTy p i e) =
  let !i'  = [modelType i]
      !e'  = [modelType e]
      ers  = (lefts  i') ++ (lefts e')
      (vli, rs0, es0) = head $ rights i'
      (vle, rs1, es1) = head $ rights e'
  in case ers of
      [] -> Right ((TA_FnTy p vli vle), (rs0 ++ rs1), (es0 ++ es1))
      er -> Left $ concat er

modelType (BzS_Cmpd p xs) =
  let !xs' = map (getCompoundContents . separateRecords) xs
      exs' = map (\(p, n, _) -> SntxErr p (n ++ " is an Enum defined in a Compound Tuple. This is not valid.")) $ catMaybes $ map checkEnum xs
      errs = (concat $ lefts xs') ++ exs'
      (as, rs, es) = (app_3_23 concat concat) $ unzip3 $ rights xs'
      (p', s, sn0) = unzip3 $ catMaybes $ map checkRecord xs
      sn1  = zip3 p' s $ map fst3 $ rights $ map modelType sn0    -- Should only work properly when no errs. Lazy evaluation kicks in then and this only runs if it's guaranteed to work.
      sn2  = map (toRecordModel "") sn1
  in case errs of
      [] -> Right ((TA_Cmpd p as), (rs ++ sn2), es)
      er -> Left er

modelType (BzS_Poly p xs) =
  let !xs' = map (getPolymorphContents . separateEnums) xs
      rxs' = map (\(p, n, _) -> SntxErr p (n ++ " is an Record defined in a Polymorphic Tuple. This is not valid.")) $ catMaybes $ map checkRecord xs
      errs = (concat $ lefts xs') ++ rxs'
      (as, rs, es) = (app_3_23 concat concat) $ unzip3 $ rights xs'
      (p', s, sn0) = unzip3 $ catMaybes $ map checkEnum xs
      sn1  = zip3 p' s $ map fst3 $ rights $ map modelType sn0    -- Should only work properly when no errs. Lazy evaluation kicks in then and this only runs if it's guaranteed to work.
      sn2  = map (toEnumModel "") sn1
  in case errs of
      [] -> Right ((TA_Poly p as), rs, (es ++ sn2))
      er -> Left er

modelType (BzS_Box p x) = modelType x

modelType (BzS_Expr p [x]) = modelType x

modelType (BzS_FilterObj p o f) =
  let !o'  = [modelType o]
      !f'  = [modelBasicType f]
      ers  = (lefts o') ++ (lefts f')
      vlf  = head $ rights f'
      (vlo, rs, es) = head $ rights o'
  in case ers of
      [] -> Right ((TA_Filt p vlf vlo), rs, es)
      er -> Left $ concat er

modelType (BzS_MapObj p o) = Left [SntxErr p "Unexpected Map Syntax in Type Expression"]

modelType (BzS_ArrayObj p o a) =
  let !o' = [modelType o]
      ers = lefts  o'
      (vls, rs, es) = head $ rights o'
      szs = map modelArrayObj a
      sze = lefts szs
      szx = rights szs
  in case (ers ++ sze) of
      [] -> Right $ ((TA_Arr p szx vls), rs, es)
      er -> Left  $ concat er

modelType (BzS_Expr p (x:xs)) =
  let !x'  = [modelType x]
      !xs' = [modelType (BzS_Expr (pos $ head xs) xs)]
      ers  = (lefts x') ++ (lefts xs')
      (vlx, rs0, es0) = head $ rights x'
      (vly, rs1, es1) = head $ rights xs'
  in case (ers) of
      [] -> Right ((TA_Expr p vlx vly), (rs0 ++ rs1), (es0 ++ es1))
      er -> Left $ concat er

modelType (BzS_CurryObj p o x) = Left [SntxErr p "Currying in type expressions is currently not permitted."]

modelType s = Left [SntxErr (pos s) "Unexpected Component of Type Expression."]









modelCalls :: BzoSyntax -> Either [BzoErr] [CallAST]
modelCalls (BzS_Calls  p xs) =
  let cs = map modelCalls xs
      er = concat $ lefts cs
      vs = concat $ rights cs
  in case er of
      []  -> Right vs
      ers -> Left ers

modelCalls (BzS_TypDef p prs t df) =
  let df' = [modelType df]
      er  = concat $ lefts  df'
      (xs, rs, es) = unzip3 $ rights df'
  in case er of
      []  -> Right [(CA_TyDefCall p t [] (concat rs) (concat es) (head xs))]  -- For now
      ers -> Left ers

modelCalls (BzS_FnTypeDef p t (BzS_FnTy _ i o)) =
  let i'  = [modelBasicType i]
      o'  = [modelBasicType o]
      er  = concat $ lefts (i' ++ o')
      ity = head $ rights i'    -- Laziness prevents errors here
      oty = head $ rights i'    --
  in case er of
      []  -> Right [(CA_FTDefCall p t ity oty)]
      ers -> Left ers









-- Add alternate version for normal compilation later. For that, take [BzoFileData] as input.
wrappedModellerMapREPL :: [BzoSyntax] -> Either [BzoErr] [[CallAST]]
wrappedModellerMapREPL ss =
  let xs = map modelCalls ss
      er = concat $ lefts  xs
      vs = rights xs
  in case er of
      []  -> Right vs
      ers -> Left ers










showEnum :: ModelEnum -> String
showEnum (ModelEnum p n r t) = " {Enum : " ++ n ++ ", inside " ++ r ++ ", of type " ++ (show t) ++ " } "
instance Show ModelEnum where show = showEnum










showRecord :: ModelRecord -> String
showRecord (ModelRecord p n r t) = " {Record : " ++ n ++ ", inside " ++ r ++ ", of type " ++ (show t) ++ " } "
instance Show ModelRecord where show = showRecord










showCallAST :: CallAST -> String
showCallAST (CA_TyDefCall p i s r e t) = " {TyDef: " ++ i ++
                                            "\n   PARS: " ++ (concatMap (\x -> (show x) ++ ", ") s) ++
                                            "\n   RECS: " ++ (concatMap (\x -> (show x) ++ ", ") r) ++
                                            "\n   ENMS: " ++ (concatMap (\x -> (show x) ++ ", ") e) ++
                                            "\n   DEF : " ++ (show t) ++ " }\n"
showCallAST (CA_FTDefCall p x i o)   = " {FnTyDef: " ++ x ++
                                            "\n   INPUT : " ++ (show i) ++
                                            "\n   OUTPUT: " ++ (show o)
instance Show CallAST where show = showCallAST










showTypeAST :: TypeAST -> String
showTypeAST (TA_Cmpd   p xs)   = " (Cmpd:\n" ++ (concatMap (\x -> (show x) ++ " .\n") xs) ++ ") "
showTypeAST (TA_Poly   p xs)   = " (Poly:\n" ++ (concatMap (\x -> (show x) ++ " ,\n") xs) ++ ") "
showTypeAST (TA_Expr   p x n)  = (show x) ++ " -> " ++ (show n)
showTypeAST (TA_Filt   p f x)  = " {" ++ (show x) ++ " : " ++ (show f) ++ "} "
showTypeAST (TA_FnTy   p i o)  = " {" ++ (show i) ++ " ;; " ++ (show o) ++ "} "
showTypeAST (TA_Enum   p i x)  = " {Enm: " ++ i ++ " of Type " ++ (show x) ++ "} "
showTypeAST (TA_Record p i x)  = " {Rcd: " ++ i ++ " of Type " ++ (show x) ++ "} "
showTypeAST (TA_Curry  p cs x) = " {Cur: " ++ (concatMap (\y -> (show y) ++ "`") cs) ++ " -> " ++ (show x) ++ "} "
showTypeAST (TA_Arr    p ss x) = " {Arr: " ++ (show x) ++ (concatMap (\n -> ife (n /= 0) ("["++(show n)++"]") ("[?]")) ss) ++ "} "
showTypeAST (TA_IntLit p i)    = " <Int: " ++ (show i) ++ "> "
showTypeAST (TA_FltLit p f)    = " <Flt: " ++ (show f) ++ "> "
showTypeAST (TA_StrLit p s)    = " <Str: " ++ s ++ "> "
showTypeAST (TA_TyLit  p x)    = " <Ty: "  ++ x ++ "> "
showTypeAST (TA_FnLit  p x)    = " <Fn: "  ++ x ++ "> "
showTypeAST (TA_BFnLit p x)    = " <BFn: " ++ x ++ "> "
showTypeAST (TA_BTyLit p x)    = " <BTy: " ++ x ++ "> "
showTypeAST (TA_TyVar  p x)    = " <TVr: " ++ x ++ "> "
showTypeAST (TA_ExTyLit p x l) = " <Ty: "  ++ x ++ ", from " ++ l ++ "> "
showTypeAST (TA_ExFnLit p x l) = " <Fn: "  ++ x ++ ", from " ++ l ++ "> "
showTypeAST (TA_Nil    p)      = " <NIL ()> "
instance Show TypeAST where show = showTypeAST
