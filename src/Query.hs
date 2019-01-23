module Query where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tuple as Tp
import Control.Parallel.Strategies
import Debug.Trace











data Environment = Environment DefinitionTable SymbolTable Context Text










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
getIds (DefinitionTable dfs files ids _) defid visible = L.filter (\x -> L.elem x visible) $ Mb.fromMaybe [] $ M.lookup defid ids










getNamespacePaths :: DefinitionTable -> FilePath -> [(Text, FilePath)]
getNamespacePaths (DefinitionTable _ files _ _) filepath =
  let filemap = M.fromList $ L.map (\file -> (bfm_filepath file, file)) files
      file    = L.filter (\f -> filepath == (bfm_filepath f)) files
      file'   = L.head file
      domain  = bfm_domain file'

      local   = L.filter (\f -> domain == (bfm_domain f)) files
      impfs   = L.filter (\f -> L.elem (bfm_moduleName f) $ bfm_fileImports file') local
      imps    = L.map (\f -> (bfm_moduleName f, bfm_filepath f)) impfs
      impfsas = L.filter (\f -> L.elem (bfm_moduleName f) $ L.map fst $ bfm_fileImportsAs file') local
      impsas  = L.map (\f -> (bfm_moduleName f, bfm_filepath f)) impfsas

      lnkfs   = L.filter (\f -> L.elem (bfm_moduleName f) $ bfm_fileLinks file') files
      lnks    = L.map (\f -> (bfm_domain     f, bfm_filepath f)) lnkfs
      lnkfsas = L.filter (\f -> L.elem (bfm_moduleName f) $ L.map fst $ bfm_fileLinksAs file') files
      lnksas  = L.map (\f -> (bfm_domain     f, bfm_filepath f)) lnkfsas

      here    = [(domain, filepath)]
  in case file of
      [] -> []
      _  -> here ++ imps ++ impsas ++ lnks ++ lnksas










getNamespaceFiles :: DefinitionTable -> FilePath -> [(Text, BzoFileModel ([Int64], [Int64]))]
getNamespaceFiles dt@(DefinitionTable _ files _ _) filepath =
  let pairs  = getNamespacePaths dt filepath
      paths  = S.fromList $ L.map snd pairs
      files' = M.fromList $ L.map (\f -> (bfm_filepath f, f)) $ L.filter (\f -> S.member (bfm_filepath f) paths) files
  in  L.map (\(ns, path) -> (ns, files' M.! path)) pairs









getVisible :: BzoFileModel ([Int64], [Int64]) -> [Int64]
getVisible model = snd $ bfm_fileModel model










getNamesFromIds :: DefinitionTable -> [Int64] -> [(Int64, Text)]
getNamesFromIds (DefinitionTable defs _ _ _) ids = L.map (\i -> (i, identifier $ Mb.fromJust $ M.lookup i defs)) ids










isType :: Definition -> Bool
isType (TypeSyntax    _ _ _) = True
isType (TyClassSyntax _ _ _) = True
isType _ = False










getTypeIds :: DefinitionTable -> Text -> Text -> [Int64]
getTypeIds (DefinitionTable defs files ids _) fname tname =
  let matchingfiles = L.filter (\fm -> (bfm_filepath fm) == (unpack fname)) files
      visible       = snd $ bfm_fileModel $ L.head matchingfiles
      matchingdefs  = Mb.fromMaybe [] $ M.lookup tname ids
  in case matchingfiles of
      [] -> []
      fs -> L.intersect visible matchingdefs










getNamespaceTags :: DefinitionTable -> FilePath -> [Int64] -> [(Int64, Text)]
getNamespaceTags dt@(DefinitionTable defs files ids _) fname visible =
  let namemap = M.fromList $ L.map Tp.swap $ getNamespacePaths dt fname
      visset  = S.fromList visible
      idpairs = M.assocs $ M.filterWithKey (\k def -> S.member k visset) defs
  in  L.map (\(i, df) -> (i, namemap M.! (unpack $ hostfile df))) idpairs










makeSymbolTable :: DefinitionTable -> FilePath -> SymbolTable
makeSymbolTable dt fp =
  let file = L.head $ L.filter (\fm -> fp == (bfm_filepath fm)) $ dt_files dt
      vis  = getVisible file
  in  (SymbolTable dt fp (M.fromList $ getNamespaceTags dt fp vis))










resolveGlobalId :: SymbolTable -> BzoSyntax -> [Int64]
resolveGlobalId (SymbolTable dt fp ids) (BzS_Id       _ fn   ) = Mb.fromMaybe [] $ M.lookup fn (dt_ids dt)
resolveGlobalId (SymbolTable dt fp ids) (BzS_TyId     _ ty   ) = Mb.fromMaybe [] $ M.lookup ty (dt_ids dt)
resolveGlobalId (SymbolTable dt fp ids) (BzS_ExFunObj _ fn ns) =
  let fn' = Mb.fromMaybe [] $ M.lookup fn (dt_ids dt)
  in  L.filter (\i -> M.member i ids) fn'
resolveGlobalId (SymbolTable dt fp ids) (BzS_ExTypObj _ ty ns) =
  let ty' = Mb.fromMaybe [] $ M.lookup ty (dt_ids dt)
  in  L.filter (\i -> M.member i ids) ty'
resolveGlobalId _ _ = []










resolveLocalId :: SymbolTable -> Context -> BzoSyntax -> [(Int64, Int64)]
resolveLocalId st ctx v@(BzS_Id  _ fn) =
  let fn' = findId ctx fn
  in case fn' of
      Just xs -> [xs]
      Nothing -> L.map (\x -> (-1, x)) $ resolveGlobalId st v
resolveLocalId st ctx   (BzS_MId _ mt) = Mb.catMaybes [findId ctx mt]
resolveLocalId st ctx other = L.map (\x -> (-1, x)) $ resolveGlobalId st other










makeNameTable :: DefinitionTable -> NameTable
makeNameTable (DefinitionTable defs files _ _) =
  let domaingroups  = L.groupBy (\a b -> (bfm_domain a) == (bfm_domain b)) files
      domaingroups' = L.map (\x -> (bfm_domain $ L.head x, x)) domaingroups
      domainmodules = L.map (\(d,ds) -> (d, M.fromList $ L.map (\x -> (bfm_moduleName x, fst $ bfm_fileModel x)) ds)) domaingroups'
      domainmodules'= M.fromList $ L.map (\(d,mp) -> (d, DomainTable mp)) domainmodules
  in  NameTable $ M.map (\(DomainTable xs) -> (DomainTable xs, L.concat $ M.elems xs)) domainmodules'










inDomain :: NameTable -> Text -> Int64 -> Bool
inDomain (NameTable nt) dm df =
  let dom = M.lookup dm nt
  in case dom of
      Nothing      -> False
      Just (_, ds) -> L.elem df ds










inModule :: NameTable -> Text -> Text -> Int64 -> Bool
inModule (NameTable nt) dm md df =
  let dom = M.lookup dm nt
  in case dom of
      Nothing -> False
      Just (DomainTable mods, _) ->
        let mdl = M.lookup md mods
        in case mdl of
            Nothing -> False
            Just ds -> L.elem df ds










getDomainVis :: NameTable -> Text -> [Int64]
getDomainVis (NameTable nt) dm =
  let dom = M.lookup dm nt
  in case dom of
      Nothing      -> []
      Just (_, ds) -> ds










getModuleVis :: NameTable -> Text -> Text -> [Int64]
getModuleVis (NameTable nt) dm md =
  let dom = M.lookup dm nt
  in case dom of
      Nothing -> []
      Just (DomainTable mods, _) ->
        let mdl = M.lookup md mods
        in case mdl of
            Nothing -> []
            Just ds -> ds
