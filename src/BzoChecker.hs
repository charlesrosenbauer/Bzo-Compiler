module BzoChecker where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Data.Set as S
import Control.Parallel.Strategies
import Debug.Trace










getTVars :: BzoSyntax -> [(Text, BzoPos)]
getTVars (BzS_TyVar      p    tvar) = [(tvar, p)]
getTVars (BzS_Expr       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Statement  _    expr) = getTVars expr
getTVars (BzS_Cmpd       _    expr) = L.concatMap getTVars expr
getTVars (BzS_Poly       _    expr) = L.concatMap getTVars expr
getTVars (BzS_FnTy       _   ax bx) = (getTVars ax) ++ (getTVars bx)
getTVars (BzS_Block      _    expr) = L.concatMap getTVars expr
getTVars (BzS_TypDef     _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_TyClassDef _ ps _ df) = (getTVars ps) ++ (L.concatMap getTVars df)
getTVars (BzS_FnTypeDef  _ ps _ df) = (getTVars ps) ++ (getTVars df)
getTVars (BzS_Calls      _      cs) = L.concatMap getTVars cs
getTVars (BzS_ArrayObj   _  _ expr) = getTVars expr
getTVars (BzS_FilterObj  _ obj  fs) = (getTVars obj) ++ (L.concatMap getTVars fs)
getTVars (BzS_CurryObj   _ obj  ps) = (getTVars obj) ++ (L.concatMap getTVars ps)
getTVars (BzS_LispCall   _ fn expr) = (getTVars fn)  ++ (L.concatMap getTVars expr)
getTVars _                          = []










getVars :: BzoSyntax -> [(Text, BzoPos)]
getVars (BzS_Id         p     var) = [(var, p)]
getVars (BzS_MId        p     var) = [(var, p)]
getVars (BzS_BId        p     var) = [(var, p)]
getVars (BzS_Expr       _    expr) = L.concatMap getVars expr
getVars (BzS_Statement  _    expr) = getVars expr
getVars (BzS_Cmpd       _    expr) = L.concatMap getVars expr
getVars (BzS_Poly       _    expr) = L.concatMap getVars expr
getVars (BzS_FnTy       _   ax bx) = (getVars ax) ++ (getVars bx)
getVars (BzS_Block      _    expr) = L.concatMap getVars expr
getVars (BzS_TypDef     _ ps _ df) = (getVars ps) ++ (getVars df)
getVars (BzS_TyClassDef _ ps _ df) = (getVars ps) ++ (L.concatMap getVars df)
getVars (BzS_FnTypeDef  _ ps _ df) = (getVars ps) ++ (getVars df)
getVars (BzS_FunDef     _ i _ o x) = (getVars i)  ++ (getVars o)  ++ (getVars x)
getVars (BzS_Calls      _      cs) = L.concatMap getVars cs
getVars (BzS_ArrayObj   _  _ expr) = getVars expr
getVars (BzS_FilterObj  _ obj  fs) = (getVars obj) ++ (L.concatMap getVars fs)
getVars (BzS_CurryObj   _ obj  ps) = (getVars obj) ++ (L.concatMap getVars ps)
getVars (BzS_MapObj     _    expr) = (getVars expr)
getVars (BzS_Lambda     _ ps expr) = (getVars ps)  ++ (getVars expr)
getVars (BzS_LispCall   _ fn expr) = (getVars fn)  ++ (L.concatMap getVars expr)
getVars _                          = []










getFuns :: BzoSyntax -> [(Text, Text, BzoPos)]
getFuns (BzS_Id         p     var) = [(var, pack "@", p)]
getFuns (BzS_MId        p     var) = [(var, pack "@", p)]
getFuns (BzS_BId        p     var) = [(var, pack "@", p)]
getFuns (BzS_ExFunObj   p var nsp) = [(var,      nsp, p)]
getFuns (BzS_Expr       _    expr) = L.concatMap getFuns expr
getFuns (BzS_Statement  _    expr) = getFuns expr
getFuns (BzS_Cmpd       _    expr) = L.concatMap getFuns expr
getFuns (BzS_Poly       _    expr) = L.concatMap getFuns expr
getFuns (BzS_FnTy       _   ax bx) = (getFuns ax) ++ (getFuns bx)
getFuns (BzS_Block      _    expr) = L.concatMap getFuns expr
getFuns (BzS_TypDef     _ ps _ df) = (getFuns ps) ++ (getFuns df)
getFuns (BzS_TyClassDef _ ps _ df) = (getFuns ps) ++ (L.concatMap getFuns df)
getFuns (BzS_FnTypeDef  _ ps _ df) = (getFuns ps) ++ (getFuns df)
getFuns (BzS_FunDef     _ i _ o x) = (getFuns i)  ++ (getFuns o)  ++ (getFuns x)
getFuns (BzS_Calls      _      cs) = L.concatMap getFuns cs
getFuns (BzS_ArrayObj   _  _ expr) = getFuns expr
getFuns (BzS_FilterObj  _ obj  fs) = (getFuns obj) ++ (L.concatMap getFuns fs)
getFuns (BzS_CurryObj   _ obj  ps) = (getFuns obj) ++ (L.concatMap getFuns ps)
getFuns (BzS_MapObj     _    expr) = (getFuns expr)
getFuns (BzS_Lambda     _ ps expr) = (getFuns ps)  ++ (getFuns expr)
getFuns (BzS_LispCall   _ fn expr) = (getFuns fn)  ++ (L.concatMap getFuns expr)
getFuns _                          = []










getTypes :: BzoSyntax -> [(Text, BzoPos)]
getTypes (BzS_TyId       p     var) = [(var, p)]
getTypes (BzS_Expr       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Statement  _    expr) = getTypes expr
getTypes (BzS_Cmpd       _    expr) = L.concatMap getTypes expr
getTypes (BzS_Poly       _    expr) = L.concatMap getTypes expr
getTypes (BzS_FnTy       _   ax bx) = (getTypes ax) ++ (getTypes bx)
getTypes (BzS_Block      _    expr) = L.concatMap getTypes expr
getTypes (BzS_TypDef     _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_TyClassDef _ ps _ df) = (getTypes ps) ++ (L.concatMap getTypes df)
getTypes (BzS_FnTypeDef  _ ps _ df) = (getTypes ps) ++ (getTypes df)
getTypes (BzS_FunDef     _ i _ o x) = (getTypes i)  ++ (getTypes o)  ++ (getTypes x)
getTypes (BzS_Calls      _      cs) = L.concatMap getTypes cs
getTypes (BzS_ArrayObj   _  _ expr) = getTypes expr
getTypes (BzS_FilterObj  _ obj  fs) = (getTypes obj) ++ (L.concatMap getTypes fs)
getTypes (BzS_CurryObj   _ obj  ps) = (getTypes obj) ++ (L.concatMap getTypes ps)
getTypes (BzS_MapObj     _    expr) = (getTypes expr)
getTypes (BzS_Lambda     _ ps expr) = (getTypes ps)  ++ (getTypes expr)
getTypes (BzS_LispCall   _ fn expr) = (getTypes fn)  ++ (L.concatMap getTypes expr)
getTypes (BzS_ExTypObj   p ty    _) = [(ty, p)]
getTypes _                          = []










getBuiltins :: BzoSyntax -> [(Text, BzoPos)]
getBuiltins (BzS_BId        p     var) = [(var, p)]
getBuiltins (BzS_BTId       p     var) = [(var, p)]
getBuiltins (BzS_Expr       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_Statement  _    expr) = getBuiltins expr
getBuiltins (BzS_Cmpd       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_Poly       _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_FnTy       _   ax bx) = (getBuiltins ax) ++ (getBuiltins bx)
getBuiltins (BzS_Block      _    expr) = L.concatMap getBuiltins expr
getBuiltins (BzS_TypDef     _ ps _ df) = (getBuiltins ps) ++ (getBuiltins df)
getBuiltins (BzS_TyClassDef _ ps _ df) = (getBuiltins ps) ++ (L.concatMap getBuiltins df)
getBuiltins (BzS_FnTypeDef  _ ps _ df) = (getBuiltins ps) ++ (getBuiltins df)
getBuiltins (BzS_FunDef     _ i _ o x) = (getBuiltins i)  ++ (getBuiltins o)  ++ (getBuiltins x)
getBuiltins (BzS_Calls      _      cs) = L.concatMap getBuiltins cs
getBuiltins (BzS_ArrayObj   _  _ expr) = getBuiltins expr
getBuiltins (BzS_FilterObj  _ obj  fs) = (getBuiltins obj) ++ (L.concatMap getBuiltins fs)
getBuiltins (BzS_CurryObj   _ obj  ps) = (getBuiltins obj) ++ (L.concatMap getBuiltins ps)
getBuiltins (BzS_MapObj     _    expr) = (getBuiltins expr)
getBuiltins (BzS_Lambda     _ ps expr) = (getBuiltins ps)  ++ (getBuiltins expr)
getBuiltins (BzS_LispCall   _ fn expr) = (getBuiltins fn)  ++ (L.concatMap getBuiltins expr)
getBuiltins _                          = []










getNamespaces :: BzoSyntax -> [(Text, BzoPos)]
getNamespaces (BzS_ExTypObj   p _    ns) = [(ns, p)]
getNamespaces (BzS_ExFunObj   p _    ns) = [(ns, p)]
getNamespaces (BzS_Expr       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_Statement  _    expr) = getNamespaces expr
getNamespaces (BzS_Cmpd       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_Poly       _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_FnTy       _   ax bx) = (getNamespaces ax) ++ (getNamespaces bx)
getNamespaces (BzS_Block      _    expr) = L.concatMap getNamespaces expr
getNamespaces (BzS_TypDef     _ ps _ df) = (getNamespaces ps) ++ (getNamespaces df)
getNamespaces (BzS_TyClassDef _ ps _ df) = (getNamespaces ps) ++ (L.concatMap getNamespaces df)
getNamespaces (BzS_FnTypeDef  _ ps _ df) = (getNamespaces ps) ++ (getNamespaces df)
getNamespaces (BzS_FunDef     _ i _ o x) = (getNamespaces i)  ++ (getNamespaces o)  ++ (getNamespaces x)
getNamespaces (BzS_Calls      _      cs) = L.concatMap getNamespaces cs
getNamespaces (BzS_ArrayObj   _  _ expr) = getNamespaces expr
getNamespaces (BzS_FilterObj  _ obj  fs) = (getNamespaces obj) ++ (L.concatMap getNamespaces fs)
getNamespaces (BzS_CurryObj   _ obj  ps) = (getNamespaces obj) ++ (L.concatMap getNamespaces ps)
getNamespaces (BzS_MapObj     _    expr) = (getNamespaces expr)
getNamespaces (BzS_Lambda     _ ps expr) = (getNamespaces ps)  ++ (getNamespaces expr)
getNamespaces (BzS_LispCall   _ fn expr) = (getNamespaces fn)  ++ (L.concatMap getNamespaces expr)
getNamespaces _                          = []










getNamespaceSet :: (Show a) => BzoFileModel a -> S.Set Text
getNamespaceSet (BzoFileModel _ _ _ _ is ls ias las) = S.fromList $ (is ++ ls) ++ (L.map snd (ias ++ las))










getVisibility :: DefinitionTable -> Text -> [Int64]
getVisibility (DefinitionTable _ files _ _) fname =
  let filematches = L.filter (\file -> fname == (pack $ bfm_filepath file)) files
  in L.concatMap (snd . bfm_fileModel) filematches










getIds :: DefinitionTable -> Text -> [Int64] -> [Int64]
getIds (DefinitionTable dfs files ids _) defid visible = Mb.fromMaybe [] $ M.lookup defid ids










getNamespaceFiles :: DefinitionTable -> FilePath -> [(Text, FilePath)]
getNamespaceFiles (DefinitionTable _ files _ _) filepath =
  let filemap = M.fromList $ L.map (\file -> (bfm_filepath file, file)) files
      file    = L.filter (\f -> filepath == (bfm_filepath f)) files
      file'   = L.head file
      domain  = bfm_domain file'

      local   = L.filter (\f -> domain == (bfm_domain f)) files
      impfs   = L.filter (\f -> L.elem (bfm_moduleName f) $ bfm_fileImports f) local
      imps    = L.map (\f -> (bfm_moduleName f, bfm_filepath f)) impfs
      impfsas = L.filter (\f -> L.elem (bfm_moduleName f) $ L.map fst $ bfm_fileImportsAs f) local
      impsas  = L.map (\f -> (bfm_moduleName f, bfm_filepath f)) impfsas

      lnkfs   = L.filter (\f -> L.elem (bfm_moduleName f) $ bfm_fileLinks f) files
      lnks    = L.map (\f -> (bfm_domain     f, bfm_filepath f)) lnkfs
      lnkfsas = L.filter (\f -> L.elem (bfm_moduleName f) $ L.map fst $ bfm_fileLinksAs f) files
      lnksas  = L.map (\f -> (bfm_domain     f, bfm_filepath f)) lnkfsas
  in case file of
      [] -> []
      _  -> imps ++ impsas ++ lnks ++ lnksas









getVisible :: BzoFileModel ([Int64], [Int64]) -> [Int64]
getVisible model = fst $ bfm_fileModel model










getNamesFromIds :: DefinitionTable -> [Int64] -> [(Int64, Text)]
getNamesFromIds (DefinitionTable defs _ _ _) ids = L.map (\i -> (i, identifier $ Mb.fromJust $ M.lookup i defs)) ids










noOverloadTypes :: DefinitionTable -> [BzoErr]
noOverloadTypes dt@(DefinitionTable defs files ids _) =
  let types  = L.filter isType $ M.elems defs
      nubbed = L.nubBy matchType types
  in case (types L.\\ nubbed) of
      [] -> []
      xs -> L.map makeOverloadErr xs

  where makeOverloadErr :: Definition -> BzoErr
        makeOverloadErr (TypeSyntax    tname _ df) = TypeErr (pos df) $ pack $ "Type "       ++ (unpack tname) ++ " is defined in multiple places."
        makeOverloadErr (TyClassSyntax tname _ df) = TypeErr (pos df) $ pack $ "Type Class " ++ (unpack tname) ++ " is defined in multiple places."

        matchType :: Definition -> Definition -> Bool
        matchType (TypeSyntax    t0 f0 _) (TypeSyntax    t1 f1 _) = (t0 == t1) && (f0 == f1)
        matchType (TyClassSyntax t0 f0 _) (TyClassSyntax t1 f1 _) = (t0 == t1) && (f0 == f1)
        matchType _ _ = False










isType :: Definition -> Bool
isType (TypeSyntax    _ _ _) = True
isType (TyClassSyntax _ _ _) = True
isType _ = False










noUndefinedErrs :: DefinitionTable -> [BzoErr]
noUndefinedErrs dt@(DefinitionTable defs files ids _) =
  let
      visiblemap :: M.Map Text [Int64]
      visiblemap = M.fromList $ L.map (\fm -> (pack $ bfm_filepath fm, snd $ bfm_fileModel fm)) files

      visdefsmap :: M.Map Text [Definition]
      visdefsmap = M.map (\vis -> L.map (\x -> Mb.fromJust $ M.lookup x defs) vis) visiblemap

      tynamesmap :: M.Map Text (S.Set Text)
      tynamesmap = M.map (S.fromList . L.map identifier . L.filter isType) visdefsmap

      filedefmap :: M.Map Text [Definition]     -- FilePath -> [Definition]
      filedefmap = insertManyList M.empty $ L.map (\df -> (hostfile df, df)) $ M.elems defs

      filetypmap :: M.Map Text [(Text, BzoPos)] -- FilePath -> [(TyId, Pos)]
      filetypmap = M.map (L.concatMap (fromDef getTypes)) filedefmap

      typeErrs :: [BzoErr]
      typeErrs = L.concat $ parMap rpar (checkScope filetypmap tynamesmap undefTyErr) $ M.keys filetypmap


      filenmsmap :: M.Map Text (S.Set Text)
      filenmsmap = M.fromList $ L.map (\fm-> (pack $ bfm_filepath fm, getNamespaceSet fm)) files

      namedefmap :: M.Map Text [(Text, BzoPos)]
      namedefmap = M.fromList $ L.map (\df-> (hostfile df, fromDef getNamespaces df)) $ M.elems defs

      nameErrs  :: [BzoErr]
      nameErrs  = L.concat $ parMap rpar (checkScope namedefmap filenmsmap undefNsErr) $ M.keys namedefmap


      -- Later I'll have to do something about getting these builtins added into the Definition table.
      bltinlist :: [(Text, BzoPos)]
      bltinlist = L.concatMap (fromDef getBuiltins) defs

      bltinErrs :: [BzoErr]
      bltinErrs = L.map (\(bi,ps)-> SntxErr ps $ append bi $ pack " is not a valid built-in.") $ L.filter (\(bi,ps)->(isBuiltinFunc bi == 0) && (isBuiltinType bi == 0)) bltinlist


  in typeErrs ++ nameErrs ++ bltinErrs
  where
        fromDef :: (Show a) => (BzoSyntax -> [a]) -> Definition -> [a]
        fromDef f (FuncSyntax _ _  ft fd) = (f ft) ++ (L.concatMap f fd)
        fromDef f (TypeSyntax _ _     td) = (f td)
        fromDef f (TyClassSyntax  _ _ cd) = (f cd)

        checkScope :: (M.Map Text [(Text, BzoPos)]) -> (M.Map Text (S.Set Text)) -> (BzoPos -> Text -> BzoErr) -> Text -> [BzoErr]
        checkScope refmap vismap mkerr fname =
          let refs = refmap M.! fname
              viss = vismap M.! fname
              missing = L.filter (\(ty,_)-> not $ S.member ty viss) refs
          in  L.map (\(ty,ps)-> mkerr ps ty) missing

        undefTyErr :: BzoPos -> Text -> BzoErr
        undefTyErr ps ty = SntxErr ps (append ty (pack " is not defined in the local scope."))

        undefNsErr :: BzoPos -> Text -> BzoErr
        undefNsErr ps ns = SntxErr ps (append ns (pack " is not a recognized namespace in the local scope."))










getTypeIds :: DefinitionTable -> Text -> Text -> [Int64]
getTypeIds (DefinitionTable defs files ids _) fname tname =
  let matchingfiles = L.filter (\fm -> (bfm_filepath fm) == (unpack fname)) files
      visible       = snd $ bfm_fileModel $ L.head matchingfiles
      matchingdefs  = Mb.fromMaybe [] $ M.lookup tname ids
  in case matchingfiles of
      [] -> []
      fs -> L.intersect visible matchingdefs










-- modelTypeExpr
  -- returns a Either Err TypeExpr
  -- checks if all type references and constructors are valid
  -- checks all type filters? maybe?

-- makeLocalScope
  -- Generates a table of all visible ids, their definitions, and types

-- buildCheckedScope
  -- takes a local scope,
  --










checkProgram :: DefinitionTable -> Either [BzoErr] DefinitionTable
checkProgram dt@(DefinitionTable defs files ids _) =
  let err0 = noOverloadTypes dt
      err1 = noUndefinedErrs dt
      errs = err0 ++ err1
  in case errs of
      [] -> Right dt
      er -> Left  er
