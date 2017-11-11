module ModelRules where
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe
import Debug.Trace










stripSyntax :: BzoSyntax -> BzoSyntax
stripSyntax (BzS_Expr p [x]) = stripSyntax x
stripSyntax (BzS_Box  p  x ) = stripSyntax x
stripSyntax x                = x










checkRecord :: BzoSyntax -> Maybe (BzoPos, String, BzoSyntax)
checkRecord (BzS_FilterObj p0 (BzS_Id  p1 i) [ty]) = Just (p0, i, ty)
checkRecord (BzS_FilterObj p0 (BzS_BId p1 i) [ty]) = Just (p0, i, ty)
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 (BzS_Id  p1 i) [ty])]) = Just (p0, i, ty)
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BId p1 i) [ty])]) = Just (p0, i, ty)
checkRecord (BzS_FilterObj p0 x@(BzS_Expr _ _) [t]) = checkRecord (BzS_FilterObj p0 (stripSyntax x) [t])
checkRecord (BzS_FilterObj p0 x@(BzS_Box  _ _) [t]) = checkRecord (BzS_FilterObj p0 (stripSyntax x) [t])
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 x@(BzS_Expr _ _) [t])]) = checkRecord (BzS_FilterObj p0 (stripSyntax x) [t])
checkRecord (BzS_Expr _ [(BzS_FilterObj p0 x@(BzS_Box  _ _) [t])]) = checkRecord (BzS_FilterObj p0 (stripSyntax x) [t])
checkRecord _                                    = Nothing










checkEnum :: BzoSyntax -> Maybe (BzoPos, String, BzoSyntax)
checkEnum (BzS_FilterObj p0 (BzS_TyId p1 i) [ty]) = Just (p0, i, ty)
checkEnum (BzS_FilterObj p0 (BzS_BTId p1 i) [ty]) = Just (p0, i, ty)
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 (BzS_TyId p1 i) [ty])]) = Just (p0, i, ty)
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BTId p1 i) [ty])]) = Just (p0, i, ty)
checkEnum (BzS_FilterObj p0 x@(BzS_Expr _ _) [t]) = checkEnum (BzS_FilterObj p0 (stripSyntax x) [t])
checkEnum (BzS_FilterObj p0 x@(BzS_Box  _ _) [t]) = checkEnum (BzS_FilterObj p0 (stripSyntax x) [t])
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 x@(BzS_Expr _ _) [t])]) = checkEnum (BzS_FilterObj p0 (stripSyntax x) [t])
checkEnum (BzS_Expr _ [(BzS_FilterObj p0 x@(BzS_Box  _ _) [t])]) = checkEnum (BzS_FilterObj p0 (stripSyntax x) [t])
checkEnum _                                     = Nothing










separateRecords :: BzoSyntax -> Either BzoSyntax (String, BzoSyntax)
separateRecords (BzS_Expr _ [(BzS_FilterObj p0 (BzS_Id  p1 i) [ty])]) = Right (i, ty)
separateRecords (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BId p1 i) [ty])]) = Right (i, ty)
separateRecords x                                    = Left  x










separateEnums :: BzoSyntax -> Either BzoSyntax (String, BzoSyntax)
separateEnums (BzS_Expr _ [(BzS_FilterObj p0 (BzS_TyId p1 i) [ty])]) = Right (i, ty)
separateEnums (BzS_Expr _ [(BzS_FilterObj p0 (BzS_BTId p1 i) [ty])]) = Right (i, ty)
separateEnums x                                     = Left  x










getCompoundContents :: String -> Either BzoSyntax (String, BzoSyntax) -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
getCompoundContents parent (Left       syn ) = modelType parent syn
getCompoundContents parent (Right (str, syn)) =
  let x  = [modelType str syn]
      ls = lefts  x
      (rs, mrs, mes) = head $ rights x
  in case ls of
      []  -> Right ((TA_Record (pos syn) str parent rs), mrs, mes)
      ers -> Left $ concat ers










getPolymorphContents :: String -> Either BzoSyntax (String, BzoSyntax) -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
getPolymorphContents parent (Left        syn ) = modelType parent syn
getPolymorphContents parent (Right (str, syn)) =
  let x  = [modelType str syn]
      ls = lefts  x
      (es, mrs, mes) = head $ rights x
  in case ls of
      []  -> Right ((TA_Enum (pos syn) str parent es), mrs, mes)
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
      ens  = catMaybes $ map checkEnum xs
      ers  = concatMap lefts  xs'
      vls  = concatMap rights xs'
  in case (ers, rcs, ens) of
      ([], [], []) -> Right (TA_Cmpd p vls)
      (er, rs, es) -> Left $ (concat er)
        ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Record Syntax: " ++ n ++ "\n")) rs)
        ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Enum Syntax: "   ++ n ++ "\n")) es)

modelBasicType (BzS_Poly p xs) =
  let !xs' = [map modelBasicType xs]
      rcs  = catMaybes $ map checkRecord xs
      ens  = catMaybes $ map checkEnum xs
      ers  = concatMap lefts  xs'
      vls  = concatMap rights xs'
  in case (ers, rcs, ens) of
      ([], [], []) -> Right (TA_Poly p vls)
      (er, rs, es) -> Left $ concat er
        ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Record Syntax: " ++ n ++ "\n")) rs)
        ++ (map (\(p, n, t) -> (SntxErr p $ "Unexpected Enum Syntax: "   ++ n ++ "\n")) es)

modelBasicType (BzS_Box p x) = modelBasicType x

modelBasicType (BzS_Expr p [x]) = modelBasicType x

modelBasicType (BzS_FilterObj p (BzS_Id _ x) _) = Left [SntxErr p "Unexpected Record"]

modelBasicType (BzS_FilterObj p (BzS_BId _ x) _) = Left [SntxErr p "Unexpected Record"]

modelBasicType (BzS_FilterObj p (BzS_BTId _ x) _) = Left [SntxErr p "Unexpected Enum"]

modelBasicType (BzS_FilterObj p (BzS_TyId _ x) _) = Left [SntxErr p "Unexpected Enum"]

modelBasicType (BzS_FilterObj p o f) =
  let !o'  = [modelBasicType o]
      !f'  = map modelBasicType f
      ers  = (lefts o') ++ (lefts f')
      vlo  = head $ rights o'
      vlf  = rights f'
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

modelBasicType (BzS_CurryObj p o x) =
  let o'  = [modelBasicType o]
      x'  = map modelBasicType x
      ers = concat $ (lefts o') ++ (lefts x')
      vlo = head $ rights o'
      vlx = rights x'
  in case ers of
      [] -> Right $ TA_Curry p vlx vlo
      er -> Left  er

modelBasicType s = Left [SntxErr (pos s) "Unexpected Component of Type Expression."]










modelType :: String -> BzoSyntax -> Either [BzoErr] (TypeAST, [ModelRecord], [ModelEnum])
modelType _ (BzS_Int   p i)  = Right ((TA_IntLit p i), [], [])
modelType _ (BzS_Flt   p f)  = Right ((TA_FltLit p f), [], [])
modelType _ (BzS_Str   p s)  = Right ((TA_StrLit p s), [], [])
modelType _ (BzS_Id    p x)  = Right ((TA_FnLit  p x), [], [])
modelType _ (BzS_TyId  p x)  = Right ((TA_TyLit  p x), [], [])
modelType _ (BzS_BId   p x)  = Right ((TA_BFnLit p x), [], [])
modelType _ (BzS_BTId  p x)  = Right ((TA_BTyLit p x), [], [])
modelType _ (BzS_Nil   p  )  = Right ((TA_Nil    p  ), [], [])
modelType _ (BzS_TyVar p x)  = Right ((TA_TyVar  p x), [], [])
modelType _ (BzS_ExFunObj p x n) = Right ((TA_ExFnLit  p x n), [], [])
modelType _ (BzS_ExTypObj p x n) = Right ((TA_ExTyLit  p x n), [], [])

modelType parent (BzS_FnTy p i e) =
  let !i'  = [modelType parent i]
      !e'  = [modelType parent e]
      ers  = (lefts  i') ++ (lefts e')
      (vli, rs0, es0) = head $ rights i'
      (vle, rs1, es1) = head $ rights e'
  in case ers of
      [] -> Right ((TA_FnTy p vli vle), (rs0 ++ rs1), (es0 ++ es1))
      er -> Left $ concat er

modelType parent (BzS_Cmpd p xs) =
  let !xs' = map ((getCompoundContents parent) . separateRecords) xs
      exs' = map (\(p, n, _) -> SntxErr p (n ++ " is an Enum defined in a Compound Tuple. This is not valid.")) $ catMaybes $ map checkEnum xs
      errs = (concat $ lefts xs') ++ exs'
      (as, rs, es) = (app_3_23 concat concat) $ unzip3 $ rights xs'
      (p', s, sn0) = unzip3 $ catMaybes $ map checkRecord xs
      sn1  = zip3 p' s $ map fst3 $ rights $ map (modelType parent) sn0    -- Should only work properly when no errs. Lazy evaluation kicks in then and this only runs if it's guaranteed to work.
      sn2  = map (toRecordModel parent) sn1
  in case errs of
      [] -> Right ((TA_Cmpd p as), (rs ++ sn2), es)
      er -> Left er

modelType parent (BzS_Poly p xs) =
  let !xs' = map ((getPolymorphContents parent) . separateEnums) xs
      rxs' = map (\(p, n, _) -> SntxErr p (n ++ " is an Record defined in a Polymorphic Tuple. This is not valid.")) $ catMaybes $ map checkRecord xs
      errs = (concat $ lefts xs') ++ rxs'
      (as, rs, es) = (app_3_23 concat concat) $ unzip3 $ rights xs'
      (p', s, sn0) = unzip3 $ catMaybes $ map checkEnum xs
      sn1  = zip3 p' s $ map fst3 $ rights $ map (modelType parent) sn0    -- Should only work properly when no errs. Lazy evaluation kicks in then and this only runs if it's guaranteed to work.
      sn2  = map (toEnumModel parent) sn1
  in case errs of
      [] -> Right ((TA_Poly p as), rs, (es ++ sn2))
      er -> Left er

modelType parent (BzS_Box p x) = modelType parent x

modelType parent (BzS_Expr p [x]) = modelType parent x

modelType _ (BzS_FilterObj p (BzS_Id _ x) _) = Left [SntxErr p "Unexpected Record"]

modelType _ (BzS_FilterObj p (BzS_BId _ x) _) = Left [SntxErr p "Unexpected Record"]

modelType _ (BzS_FilterObj p (BzS_BTId _ x) _) = Left [SntxErr p "Unexpected Enum"]

modelType _ (BzS_FilterObj p (BzS_TyId _ x) _) = Left [SntxErr p "Unexpected Enum"]

modelType parent (BzS_FilterObj p o f) =
  let !o'  = [modelType parent o]
      !f'  = map modelBasicType f
      ers  = (lefts o') ++ (lefts f')
      vlf  = rights f'
      (vlo, rs, es) = head $ rights o'
  in case ers of
      [] -> Right ((TA_Filt p vlf vlo), rs, es)
      er -> Left $ concat er

modelType _ (BzS_MapObj p o) = Left [SntxErr p "Unexpected Map Syntax in Type Expression"]

modelType parent (BzS_ArrayObj p o a) =
  let !o' = [modelType parent o]
      ers = lefts  o'
      (vls, rs, es) = head $ rights o'
      szs = map modelArrayObj a
      sze = lefts szs
      szx = rights szs
  in case (ers ++ sze) of
      [] -> Right $ ((TA_Arr p szx vls), rs, es)
      er -> Left  $ concat er

modelType parent (BzS_Expr p (x:xs)) =
  let !x'  = [modelBasicType x]
      !xs' = [modelBasicType (BzS_Expr (pos $ head xs) xs)]
      ers  = (lefts x') ++ (lefts xs')
      vlx = head $ rights x'
      vly = head $ rights xs'
  in case (ers) of
      [] -> Right ((TA_Expr p vlx vly), [], [])
      er -> Left $ concat er

modelType parent (BzS_CurryObj p o x) =
  let o'  = [modelBasicType o]
      x'  = map modelBasicType x
      ers = concat $ (lefts o') ++ (lefts x')
      vlo = head $ rights o'
      vlx = rights x'
  in case ers of
      [] -> Right $ ((TA_Curry p vlx vlo), [], [])
      er -> Left  er

modelType _ s = Left [SntxErr (pos s) "Unexpected Component of Type Expression."]










modelTPars :: BzoSyntax -> Either [BzoErr] TParModel
modelTPars (BzS_TyVar     p x   ) = Right (TParVar p x [])
modelTPars (BzS_FilterObj p (BzS_TyVar _ x) f ) =
  let f' = map modelBasicType f
      fl = lefts f'
      fr = rights f'
  in case fl of
      [] -> Right (TParVar p x fr)
      er -> Left $ concat fl

modelTPars (BzS_Expr      p [x] ) = modelTPars x
modelTPars (BzS_Box       p x   ) = modelTPars x
modelTPars (BzS_Poly      p _   ) = Left [SntxErr p "Unexpected Polymorphic Expression as Type Parameters"]
modelTPars (BzS_Cmpd      p xs  ) =
  let xs' = map modelTPars xs
      xrs = rights xs'
      xls = lefts  xs'
  in case xls of
      [] -> Right (TParModel p xrs)
      er -> Left  $ concat er

modelTPars (BzS_Undefined)        = Right TParNil
modelTPars x                      = Left [SntxErr (pos x) "Invalid Definition of Type Parameter"]










modelFPars :: BzoSyntax -> Either [BzoErr] FParModel
modelFPars (BzS_Id        p x   ) = Right (FParVar p x  )
modelFPars (BzS_TyId      p x   ) = Right (FParTyp p x  )
modelFPars (BzS_Int       p i   ) = Right (FParInt p i  )
modelFPars (BzS_Flt       p f   ) = Right (FParFlt p f  )
modelFPars (BzS_Str       p s   ) = Right (FParStr p s  )
modelFPars (BzS_Nil       p     ) = Right (FParNilVal p )
modelFPars (BzS_Wildcard  p     ) = Right (FParWild   p )
modelFPars (BzS_FilterObj p x f ) =
  let f' = map modelBasicType f
      x' = [modelFPars x]
      fl = (lefts f') ++ (lefts x')
      fr = rights f'
      xr = head $ rights x'
  in case fl of
      [] -> Right (FParFilt p xr fr)
      er -> Left $ concat fl

modelFPars (BzS_Expr      p [x] ) = modelFPars x
modelFPars (BzS_Box       p x   ) = modelFPars x
modelFPars (BzS_Poly      p _   ) = Left [SntxErr p "Unexpected Polymorphic Expression as Function Parameters"]
modelFPars (BzS_Cmpd      p xs  ) =
  let xs' = map modelFPars xs
      xrs = rights xs'
      xls = lefts  xs'
  in case xls of
      [] -> Right (FParModel p xrs)
      er -> Left  $ concat er

modelFPars (BzS_Undefined)        = Right FParNil
modelFPars x                      = Left [SntxErr (pos x) "Invalid Definition of Function Parameter"]










isValidHintPar :: BzoSyntax -> Either BzoErr ExprModel
isValidHintPar (BzS_Id   p i) = Right $ EM_Id     p i
isValidHintPar (BzS_BId  p i) = Right $ EM_BId    p i
isValidHintPar (BzS_TyId p i) = Right $ EM_TyId   p i
isValidHintPar (BzS_BTId p i) = Right $ EM_BTyId  p i
isValidHintPar (BzS_Int  p i) = Right $ EM_LitInt p i
isValidHintPar (BzS_Flt  p f) = Right $ EM_LitFlt p f
isValidHintPar (BzS_Str  p s) = Right $ EM_LitStr p s
isValidHintPar (BzS_Expr p [x]) = isValidHintPar x
isValidHintPar (BzS_Box  p   x) = isValidHintPar x
isValidHintPar bzs            = Left  $ SntxErr (pos bzs) "Invalid Hint Parameter"









modelExpr :: BzoSyntax -> Either [BzoErr] ExprModel
modelExpr (BzS_MId      p i  ) = Right $ EM_MId      p i
modelExpr (BzS_Id       p i  ) = Right $ EM_Id       p i
modelExpr (BzS_TyId     p i  ) = Right $ EM_TyId     p i
modelExpr (BzS_BId      p i  ) = Right $ EM_BId      p i
modelExpr (BzS_BTId     p i  ) = Right $ EM_BTyId    p i
modelExpr (BzS_Int      p i  ) = Right $ EM_LitInt   p i
modelExpr (BzS_Flt      p f  ) = Right $ EM_LitFlt   p f
modelExpr (BzS_Str      p s  ) = Right $ EM_LitStr   p s
modelExpr (BzS_ExFunObj p i l) = Right $ EM_ExFun    p i l
modelExpr (BzS_ExTypObj p i l) = Right $ EM_ExTyp    p i l
modelExpr (BzS_Wildcard p    ) = Right $ EM_Wildcard p
modelExpr (BzS_Nil      p    ) = Right $ EM_Nil      p
modelExpr (BzS_Box      p   x) = modelExpr x

modelExpr (BzS_Expr   p [x]) = modelExpr x

modelExpr (BzS_CurryObj p x crs) =
  let x'   = [modelExpr x]
      crs' = map modelExpr crs
      ers = concat $ (lefts x') ++ (lefts crs')
  in case ers of
      [] -> Right $ EM_Curry p (rights crs') (head $ rights x')
      er -> Left  er

modelExpr (BzS_Lambda p prs df) =
  let df'  = [modelExpr df]
      prs' = [modelFPars prs]
      ers  = (lefts df') ++ (lefts prs')
  in case ers of
      [] -> Right $ EM_Lambda p (head $ rights prs') (head $ rights df')
      er -> Left  $ concat er

modelExpr (BzS_FilterObj p x filt) =
  let x'   = [modelExpr x]
      flt' = map modelBasicType filt
      ers  = (lefts x') ++ (lefts flt')
  in case ers of
      [] -> Right $ EM_Filt p (head $ rights x') (rights flt')
      er -> Left  $ concat er

modelExpr (BzS_MapObj p x ) =
  let x' = modelExpr x
  in case (lefts [x']) of
      [] -> Right $ EM_Map p $ head $ rights [x']
      er -> Left  $ concat er

modelExpr (BzS_Block  p xs ) =
  let xs' = map modelExpr xs
      xsl = lefts  xs'
      xsr = rights xs'
  in case xsl of
      [] -> Right $ EM_Block p xsr
      er -> Left  $ concat er

modelExpr (BzS_Cmpd   p xs) =
  let xs' = map modelExpr xs
      xsl = lefts  xs'
      xsr = rights xs'
  in case xsl of
      [] -> Right $ EM_Cmpd p xsr
      er -> Left $ concat er

modelExpr (BzS_Poly   p xs) =
  let xs' = map modelExpr xs
      xsl = lefts  xs'
      xsr = rights xs'
  in case xsl of
      [] -> Right $ EM_Poly p xsr
      er -> Left $ concat er

modelExpr (BzS_Expr p [(BzS_Cmpd _ xs), (BzS_BId _ hint)]) =
  let xs' = map isValidHintPar xs
  in case (lefts xs') of
      [] -> Right $ EM_Hint p hint (rights xs')
      er -> Left  er

modelExpr (BzS_Expr p [(BzS_Cmpd _ xs), (BzS_BTId _ hint)]) =
  let xs' = map isValidHintPar xs
  in case (lefts xs') of
      [] -> Right $ EM_Hint p hint (rights xs')
      er -> Left  er

modelExpr (BzS_Expr p [x, (BzS_BId _ hint)]) =
  let xs' = [isValidHintPar x]
  in case (lefts xs') of
      [] -> Right $ EM_Hint p hint (rights xs')
      er -> Left  er

modelExpr (BzS_Expr p [x, (BzS_BTId _ hint)]) =
  let xs' = [isValidHintPar x]
  in case (lefts xs') of
      [] -> Right $ EM_Hint p hint (rights xs')
      er -> Left  er

modelExpr (BzS_Expr   p (x:xs)) =
  let x'  = [modelExpr x ]
      xs' = [modelExpr (BzS_Expr (pos $ head xs) xs)]
      ers = concat $ lefts (x' ++ xs')
  in case ers of
      [] -> Right $ EM_Expr p (head $ rights x') (head $ rights xs')
      er -> Left er










modelCalls :: BzoSyntax -> Either [BzoErr] [CallAST]
modelCalls (BzS_Calls  p xs) =
  let cs = map modelCalls xs
      er = concat $ lefts cs
      vs = concat $ rights cs
  in case er of
      []  -> Right vs
      ers -> Left ers

modelCalls (BzS_TypDef p prs t df) =
  let df' = [modelType t df]
      ps' = [modelTPars prs]
      psr = head $ rights ps'
      er  = (concat $ lefts  df') ++ (concat $ lefts ps')
      (xs, rs, es) = unzip3 $ rights df'
      rs' = map (\(ModelRecord rp ri _ rt) -> (ModelRecord rp ri t rt)) $ concat rs
      es' = map (\(ModelEnum   ep ei _ et) -> (ModelEnum   ep ei t et)) $ concat es
  in case er of
      [] -> Right [(CA_TypeDefCall p t psr rs' es' (head xs))]
      ers -> Left ers

modelCalls (BzS_FnTypeDef p fnid (BzS_FnTy _ i o)) =
  let i'  = [modelBasicType i]
      o'  = [modelBasicType o]
      er  = concat $ lefts (i' ++ o')
      ity = head $ rights i'    -- Laziness prevents errors here
      oty = head $ rights o'    --
  in case er of
      []  -> Right [(CA_FTDefCall p fnid ity oty)]
      ers -> Left ers

modelCalls (BzS_FunDef p i f e x) =
  let x' = [modelExpr x]
      i' = [modelFPars i]
      e' = [modelFPars e]
      er = (lefts x') ++ (lefts i') ++ (lefts e')
      ri = head $ rights i'
      re = head $ rights e'
  in case er of
      [] -> Right [(CA_FnDefCall p f ri re (head $ rights x'))]
      ers -> Left $ concat ers

modelCalls (BzS_Expr p [(BzS_Cmpd _ xs), (BzS_BId _ hint)]) =
  let xs' = map isValidHintPar xs
  in case (lefts xs') of
      [] -> Right [CA_HintCall p hint (rights xs')]
      er -> Left  er

modelCalls (BzS_Expr p [(BzS_Cmpd _ xs), (BzS_BTId _ hint)]) =
  let xs' = map isValidHintPar xs
  in case (lefts xs') of
      [] -> Right [CA_HintCall p hint (rights xs')]
      er -> Left  er

modelCalls (BzS_Expr p [x, (BzS_BId _ hint)]) =
  let xs' = [isValidHintPar x]
  in case (lefts xs') of
      [] -> Right [CA_HintCall p hint (rights xs')]
      er -> Left  er

modelCalls x = Left [SntxErr (pos x) $ "Unexpected Expression Call: " ++ (show x)]










modelREPLCalls :: BzoSyntax -> Either [BzoErr] CallAST
modelREPLCalls sntx =
  let sntx' = [modelCalls sntx]
      errs  = lefts sntx'
      snxrs = head $ rights sntx'
  in case (errs, sntx) of
      ([], _ ) -> Right $ CA_Calls (ca_pos $ head $ snxrs) snxrs
      (er, (BzS_Calls _ [sn@(BzS_Expr p _)])) ->
        case (modelExpr sn) of
          Left er -> Left er
          Right x -> Right $ CA_REPLCall p x
      (er, _ ) -> Left $ concat er









wrappedModellerMapREPL :: [BzoSyntax] -> Either [BzoErr] [CallAST]
wrappedModellerMapREPL ss =
  let xs = map modelREPLCalls ss
      er = concat $ lefts  xs
      vs = rights xs
  in case er of
      []  -> Right vs
      ers -> Left ers










wrappedModellerMap :: [BzoFileModel BzoSyntax] -> Either [BzoErr] [BzoFileModel CallAST]
wrappedModellerMap ss =
  let xs = map (modelCalls . bfm_fileModel) ss
      er = concat $ lefts  xs
      vs = rights xs
      rets = map adjustAST $ zip ss $ map (\xs -> CA_Calls (ca_pos $ head xs) xs) vs
  in case er of
      []  -> Right rets
      ers -> Left ers
  where adjustAST :: Show a => (BzoFileModel a, CallAST) -> BzoFileModel CallAST
        adjustAST ((BzoFileModel mn fp dm _ fi fl fia fla), ast) = (BzoFileModel mn fp dm ast fi fl fia fla)
