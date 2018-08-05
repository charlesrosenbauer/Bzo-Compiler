module ModelRules where
import BzoTypes
import HigherOrder
import Data.Either
import Data.Maybe
import Data.Text
import Data.Tuple
import Data.Int
import Data.Map.Strict as M
import Data.List as L
import DefinitionTable
import Debug.Trace










zeroContext :: Context
zeroContext = Context M.empty M.empty M.empty M.empty 0










addVar :: Context -> Text -> [TyId] -> (Context, Int64)
addVar cx@(Context tvs vs ts fs top) var tyids =
  let vsLookup = L.map (\(a, (b, c)) -> (c, a)) $ M.assocs vs
  in case (L.lookup var vsLookup) of
      Just x  ->  (cx, x)
      Nothing -> ((Context tvs (M.insert (top+1) (tyids, var) vs) ts fs (top+1)), top+1)










addTVr :: Context -> Text -> [TyId] -> (Context, Int64)
addTVr cx@(Context tvs vs ts fs top) var tyids =
  let tvLookup = L.map (\(a, (b, c)) -> (c, a)) $ M.assocs tvs
  in case (L.lookup var tvLookup) of
      Just x  ->  (cx, x)
      Nothing -> ((Context (M.insert (top+1) (tyids, var) tvs) vs ts fs (top+1)), top+1)










addType :: Context -> TyId -> (Context, Int64)
addType cx@(Context tvs vs ts fs top) newtyp =
  let tyLookup = L.map swap $ M.assocs ts
  in case (L.lookup newtyp tyLookup) of
      Just x  ->  (cx, x)
      Nothing -> ((Context tvs vs (M.insert (top+1) newtyp ts) fs (top+1)), top+1)










addFunc :: Context -> FnId -> (Context, Int64)
addFunc cx@(Context tvs vs ts fs top) newfun =
  let fnLookup = L.map swap $ M.assocs fs
  in case (L.lookup newfun fnLookup) of
      Just x  ->  (cx, x)
      Nothing -> ((Context tvs vs ts (M.insert (top+1) newfun fs) (top+1)), top+1)










getTVars :: BzoSyntax -> [Text]
getTVars (BzS_TyVar      _    tvar) = [pack tvar]
getTVars (BzS_Expr       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Box        _    expr) = getTVars expr
getTVars (BzS_Cmpd       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Poly       _    expr) = L.concatMap getTVars expr
getTVars (BzS_FnTy       _   ax bx) = (getTVars ax) ++ (getTVars bx)
getTVars (BzS_Block      _    expr) = L.concatMap getTVars expr
getTVars (BzS_TypDef     _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_TyClassDef _ ps _ df) = (getTVars ps) ++ (L.concatMap getTVars df)
getTVars (BzS_FnTypeDef  _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_Calls      _      cs) = L.concatMap getTVars cs
getTVars (BzS_ArrayObj   _ expr _ ) = getTVars expr
getTVars (BzS_FilterObj  _ obj  fs) = (getTVars obj) ++ (L.concatMap getTVars fs)
getTVars (BzS_CurryObj   _ obj  ps) = (getTVars obj) ++ (L.concatMap getTVars ps)
getTVars _                          = []










getVars :: BzoSyntax -> [Text]
getVars (BzS_Id         _     var) = [pack var]
getVars (BzS_MId        _     var) = [pack var]
getVars (BzS_BId        _     var) = [pack var]
getVars (BzS_Expr       _    expr) = L.concatMap getVars expr
getVars (BzS_Box        _    expr) = getVars expr
getVars (BzS_Cmpd       _    expr) = L.concatMap getVars expr
getVars (BzS_Poly       _    expr) = L.concatMap getVars expr
getVars (BzS_FnTy       _   ax bx) = (getVars ax) ++ (getVars bx)
getVars (BzS_Block      _    expr) = L.concatMap getVars expr
getVars (BzS_TypDef     _ ps _ df) = (getVars ps) ++ (getVars df)
getVars (BzS_TyClassDef _ ps _ df) = (getVars ps) ++ (L.concatMap getVars df)
getVars (BzS_FnTypeDef  _ ps _ df) = (getVars ps) ++ (getVars df)
getVars (BzS_FunDef     _ i _ o x) = (getVars i)  ++ (getVars o)  ++ (getVars x)
getVars (BzS_Calls      _      cs) = L.concatMap getVars cs
getVars (BzS_ArrayObj   _ expr _ ) = getVars expr
getVars (BzS_FilterObj  _ obj  fs) = (getVars obj) ++ (L.concatMap getVars fs)
getVars (BzS_CurryObj   _ obj  ps) = (getVars obj) ++ (L.concatMap getVars ps)
getVars (BzS_MapObj     _    expr) = (getVars expr)
getVars (BzS_Lambda     _ ps expr) = (getVars ps)  ++ (getVars expr)
getVars _                          = []










getTypes :: BzoSyntax -> [Text]
getTypes (BzS_TyId       _     var) = [pack var]
getTypes (BzS_BTId       _     var) = [pack var]
getTypes (BzS_Expr       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Box        _    expr) = getTypes expr
getTypes (BzS_Cmpd       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Poly       _    expr) = L.concatMap getTypes expr
getTypes (BzS_FnTy       _   ax bx) = (getTypes ax) ++ (getTypes bx)
getTypes (BzS_Block      _    expr) = L.concatMap getTypes expr
getTypes (BzS_TypDef     _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_TyClassDef _ ps _ df) = (getTypes ps) ++ (L.concatMap getTypes df)
getTypes (BzS_FnTypeDef  _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_FunDef     _ i _ o x) = (getTypes i)  ++ (getTypes o)  ++ (getTypes x)
getTypes (BzS_Calls      _      cs) = L.concatMap getTypes cs
getTypes (BzS_ArrayObj   _ expr _ ) = getTypes expr
getTypes (BzS_FilterObj  _ obj  fs) = (getTypes obj) ++ (L.concatMap getTypes fs)
getTypes (BzS_CurryObj   _ obj  ps) = (getTypes obj) ++ (L.concatMap getTypes ps)
getTypes (BzS_MapObj     _    expr) = (getTypes expr)
getTypes (BzS_Lambda     _ ps expr) = (getTypes ps)  ++ (getTypes expr)
getTypes _                          = []










divideIntoDefs :: [BzoSyntax] -> [Definition]
divideIntoDefs [BzS_Calls _ cs] = divideIntoDefs $ L.reverse cs
divideIntoDefs [BzS_File  _ _ _ _ _ dfs] = divideIntoDefs $ L.reverse dfs
divideIntoDefs asts = L.foldl divideDefStep [] asts
  where divideDefStep :: [Definition] -> BzoSyntax -> [Definition]
        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FunDef p _ fnid' _ _) =
          if fnid == (pack fnid')
            then ((FuncSyntax       fnid   file fty (fd:fdfs)):defs)
            else ((FuncSyntax (pack fnid') (pack $ fileName p) BzS_Undefined [fd]):f:defs)

        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax (pack fnid') (pack $ fileName p) fd []):f:defs)

        divideDefStep defs fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax (pack fnid') (pack $ fileName p) fd []):defs)

        divideDefStep defs td@(BzS_TypDef p _ tyid _) =
          ((TypeSyntax (pack tyid) (pack $ fileName p) td):defs)

        divideDefStep defs td@(BzS_TyClassDef p _ tyid _) =
          ((TyClassSyntax (pack tyid) (pack $ fileName p) td):defs)










getFnIds :: [Definition] -> [Text]
getFnIds ((FuncDef    fnid   _ _):defs) = (fnid):(getFnIds defs)
getFnIds ((FuncSyntax fnid _ _ _):defs) = (fnid):(getFnIds defs)
getFnIds (_:defs)                    = getFnIds defs










getTyIds :: [Definition] -> [Text]
getTyIds ((TypeDef       tyid _ _):defs) = (tyid):(getTyIds defs)
getTyIds ((TypeSyntax    tyid _ _):defs) = (tyid):(getTyIds defs)
getTyIds ((TyClassSyntax tyid _ _):defs) = (tyid):(getTyIds defs)
getTyIds (_:defs)                        = getTyIds defs










getDefs :: [BzoFileModel BzoSyntax] -> [BzoFileModel [Definition]]
getDefs [] = []
getDefs ((BzoFileModel mn fp dm model is ls ia la):fs) = ((BzoFileModel mn fp dm (divideIntoDefs [model]) is ls ia la):(getDefs fs))









{-
makeContext :: Definition -> Context
makeContext (FuncSyntax fnid file fty fsyns) =
  let types = (getTypes fty) ++ (getTypes fsyns)
      vars  = (getVars fsyns)
      tvars = (getTVars fty) ++ (getTVars fsyns)
  in -}
