module BzoChecker where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
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










getTypes :: BzoSyntax -> [(Text, BzoPos)]
getTypes (BzS_TyId       p     var) = [(var, p)]
getTypes (BzS_BTId       p     var) = [(var, p)]
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
        makeOverloadErr (TypeSyntax tname _ df) = TypeErr (pos df) $ pack $ (unpack tname) ++ " is defined in multiple places."

        matchType :: Definition -> Definition -> Bool
        matchType (TypeSyntax t0 f0 _) (TypeSyntax t1 f1 _) = (t0 == t1) && (f0 == f1)
        matchType _ _ = False

        isType :: Definition -> Bool
        isType (TypeSyntax _ _ _) = True
        isType _ = False










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
  in case err0 of
      [] -> Right dt
      er -> Left  er
