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










{-
data Environment = Environment DefinitionTable SymbolTable Context Text
-}









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










-- Get variables, but stop when a new subscope is reached (lambdas, blocks)
getScopeVars :: BzoSyntax -> [(Text, BzoPos)]
getScopeVars (BzS_Id         p     var) = [(var, p)]
getScopeVars (BzS_MId        p     var) = [(var, p)]
getScopeVars (BzS_Expr       _    expr) = L.concatMap getScopeVars expr
getScopeVars (BzS_Statement  _    expr) = getScopeVars expr
getScopeVars (BzS_Cmpd       _    expr) = L.concatMap getScopeVars expr
getScopeVars (BzS_Poly       _    expr) = L.concatMap getScopeVars expr
getScopeVars (BzS_FnTy       _   ax bx) = (getScopeVars ax) ++ (getScopeVars bx)
getScopeVars (BzS_ArrayObj   _  _ expr) = getScopeVars expr
getScopeVars (BzS_FilterObj  _ obj  fs) = (getScopeVars obj) ++ (L.concatMap getScopeVars fs)
getScopeVars (BzS_CurryObj   _ obj  ps) = (getScopeVars obj) ++ (L.concatMap getScopeVars ps)
getScopeVars (BzS_MapObj     _    expr) = (getScopeVars expr)
getScopeVars (BzS_LispCall   _ fn expr) = (getScopeVars fn)  ++ (L.concatMap getScopeVars expr)
getScopeVars _                          = []









-- Get all blocks and lambdas referenced in the current scope. Do not introspect them further.
getSubScopes :: BzoSyntax -> [(BzoPos, BzoSyntax)]
getSubScopes (BzS_Expr       _    expr) = L.concatMap getSubScopes expr
getSubScopes (BzS_Statement  _    expr) = getSubScopes expr
getSubScopes (BzS_Cmpd       _    expr) = L.concatMap getSubScopes expr
getSubScopes (BzS_Poly       _    expr) = L.concatMap getSubScopes expr
getSubScopes (BzS_FnTy       _   ax bx) = (getSubScopes ax) ++ (getSubScopes bx)
getSubScopes (BzS_ArrayObj   _  _ expr) = getSubScopes expr
getSubScopes (BzS_FilterObj  _ obj  fs) = (getSubScopes obj) ++ (L.concatMap getSubScopes fs)
getSubScopes (BzS_CurryObj   _ obj  ps) = (getSubScopes obj) ++ (L.concatMap getSubScopes ps)
getSubScopes (BzS_MapObj     _    expr) = (getSubScopes expr)
getSubScopes (BzS_Lambda     p ps expr) = [(p, BzS_Lambda     p ps expr)]
getSubScopes (BzS_Block      p    expr) = [(p, BzS_Block      p    expr)]
getSubScopes (BzS_LispCall   _ fn expr) = (getSubScopes fn)  ++ (L.concatMap getSubScopes expr)
getSubScopes (BzS_TyClassDef _ ps _ df) = (getSubScopes ps) ++ (L.concatMap getSubScopes df)
getSubScopes (BzS_FunDef     _ _ _ _ x) = (getSubScopes x)
getSubScopes _                          = []










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










ctScopes :: BzoSyntax -> Int
ctScopes (BzS_Id         p     var) = 0
ctScopes (BzS_MId        p     var) = 0
ctScopes (BzS_BId        p     var) = 0
ctScopes (BzS_ExFunObj   p var nsp) = 0
ctScopes (BzS_Expr       _    expr) = sum $ L.map ctScopes expr
ctScopes (BzS_Statement  _    expr) = ctScopes expr
ctScopes (BzS_Cmpd       _    expr) = sum $ L.map ctScopes expr
ctScopes (BzS_Poly       _    expr) = sum $ L.map ctScopes expr
ctScopes (BzS_FnTy       _   ax bx) = (ctScopes ax) + (ctScopes bx)
ctScopes (BzS_Block      _    expr) = 1 + (sum $ L.map ctScopes expr)
ctScopes (BzS_TypDef     _ ps _ df) = (ctScopes ps) + (ctScopes df)
ctScopes (BzS_TyClassDef _ ps _ df) = (ctScopes ps) + (sum $ L.map ctScopes df)
ctScopes (BzS_FnTypeDef  _ ps _ df) = (ctScopes ps) + (ctScopes df)
ctScopes (BzS_FunDef     _ i _ o x) = (ctScopes i)  + (ctScopes o)  + (ctScopes x)
ctScopes (BzS_Calls      _      cs) = sum $ L.map ctScopes cs
ctScopes (BzS_ArrayObj   _  _ expr) = ctScopes expr
ctScopes (BzS_FilterObj  _ obj  fs) = (ctScopes obj) + (sum $ L.map ctScopes fs)
ctScopes (BzS_CurryObj   _ obj  ps) = (ctScopes obj) + (sum $ L.map ctScopes ps)
ctScopes (BzS_MapObj     _    expr) = (ctScopes expr)
ctScopes (BzS_Lambda     _ ps expr) = (ctScopes ps)  + (ctScopes expr)
ctScopes (BzS_LispCall   _ fn expr) = (ctScopes fn)  + (sum $ L.map ctScopes expr)
ctScopes _                          = 0










ctDefScopes :: Definition -> Int
ctDefScopes (FuncSyntax _ _ _ def) = 1 + (L.length def) + (sum $ L.map ctScopes def)
ctDefScopes (TypeSyntax _ _ _    ) = 1
ctDefScopes (TyClassSyntax  _ _ _) = 1










getNamespaceSet :: (Show a) => BzoFileModel a -> S.Set Text
getNamespaceSet (BzoFileModel _ _ _ _ is ls ias las) = S.fromList $ (is ++ ls) ++ (L.map snd (ias ++ las))









{-
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

-}







getVisible :: BzoFileModel ([Int64], [Int64]) -> [Int64]
getVisible model = snd $ bfm_fileModel model

{-








getNamesFromIds :: DefinitionTable -> [Int64] -> [(Int64, Text)]
getNamesFromIds (DefinitionTable defs _ _ _) ids = L.map (\i -> (i, identifier $ Mb.fromJust $ M.lookup i defs)) ids








-}

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








{-

getNamespaceTags :: DefinitionTable -> FilePath -> [Int64] -> [(Int64, Text)]
getNamespaceTags dt@(DefinitionTable defs files ids _) fname visible =
  let namemap = M.fromList $ L.map Tp.swap $ getNamespacePaths dt fname
      visset  = S.fromList visible
      idpairs = M.assocs $ M.filterWithKey (\k def -> S.member k visset) defs
  in  L.map (\(i, df) -> (i, namemap M.! (unpack $ hostfile df))) idpairs
-}









makeSymbolTable :: DefinitionTable -> FilePath -> SymbolTable
makeSymbolTable dt fp =
  let file = L.head $ L.filter (\fm -> fp == (bfm_filepath fm)) $ dt_files dt
      vis  = getVisible file
  in  (SymbolTable dt fp M.empty)--(M.fromList $ getNamespaceTags dt fp vis))










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
{-









resolveLocalId :: SymbolTable -> Context -> BzoSyntax -> [(Int64, Int64)]
resolveLocalId st ctx v@(BzS_Id  _ fn) =
  let fn' = findId ctx fn
  in case fn' of
      Just xs -> [xs]
      Nothing -> L.map (\x -> (-1, x)) $ resolveGlobalId st v
resolveLocalId st ctx   (BzS_MId _ mt) = Mb.catMaybes [findId ctx mt]
resolveLocalId st ctx other = L.map (\x -> (-1, x)) $ resolveGlobalId st other









-}
makeNameTable :: DefinitionTable -> NameTable
makeNameTable (DefinitionTable defs files _ _) =
  let domaingroups  = L.groupBy (\a b -> (bfm_domain a) == (bfm_domain b)) files
      domaingroups' = L.map (\x -> (bfm_domain $ L.head x, x)) domaingroups
      domainmodules = L.map (\(d,ds) -> (d, M.fromList $ L.map (\x -> (bfm_moduleName x, fst $ bfm_fileModel x)) ds)) domaingroups'
      domainmodules'= M.fromList $ L.map (\(d,mp) -> (d, DomainTable mp)) domainmodules
  in  NameTable $ M.map (\(DomainTable xs) -> (DomainTable xs, L.concat $ M.elems xs)) domainmodules'
{-









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
-}









initializeScopeTable :: Int -> ScopeTable
initializeScopeTable ct = ScopeTable (M.fromList $ L.take ct $ L.zip [1..] (L.repeat $ Scope (M.empty) (M.empty) [(pack "@", [1])])) ct










insertScopeObj :: ScopeTable -> Int -> Int -> Text-> ScopeObj -> ScopeTable
insertScopeObj (ScopeTable sts sz) sc ix nm obj =
  let st = M.lookup sc sts
  in case st of
      Just (Scope s ns ps) -> ScopeTable (M.insert sc (Scope (M.insert ix obj s) (insertMapList ns nm ix) ps) sts) sz
      Nothing              -> ScopeTable sts sz










lookupScopeMap :: ScopeTable -> Int -> Maybe Scope
lookupScopeMap (ScopeTable sts _) sc = M.lookup sc sts










lookupScopeObj :: ScopeTable -> Int -> Int -> Maybe ScopeObj
lookupScopeObj sctab sc ix =
  let smap = lookupScopeMap sctab sc
  in case smap of
      Nothing              -> Nothing
      Just (Scope obs _ _) -> M.lookup ix obs










lookupScopeParents :: ScopeTable -> Int -> [Int]
lookupScopeParents sctab i =
  case (lookupScopeMap sctab i) of
    Just (Scope _ _ ps) -> L.concatMap snd ps
    Nothing             -> []









-- TEST: Make sure recursive lookup works properly.
lookupScopeName :: ScopeTable -> Int -> Text -> [Int]
lookupScopeName sctab sc nm =
  let smap = lookupScopeMap sctab sc
  in case smap of
      Nothing               -> []
      Just (Scope _ nms ps) ->
        case (M.lookup nm nms) of
          Nothing -> L.concatMap (\i->lookupScopeName sctab i nm) $ L.concatMap snd ps
          Just xs -> xs











makeScopeTable :: DefinitionTable -> (ScopeTable, M.Map Text Int)
makeScopeTable dt@(DefinitionTable dfs fs ids _) =
  let
      modeldef :: (Int, Definition) -> (Text, ScopeObj)
      modeldef = (\(i, df) -> case df of
                    (FuncDef  _ h _ _ _ _) -> (h, (Sc_Func i A_InvalidType []))
                    (TypeDef    _ h _ _ _) -> (h, (Sc_Type i A_InvalidType  0))
                    (TyClassDef _ h _ _ _) -> (h, (Sc_TyCs i A_InvalidType  0))
                    (FuncSyntax   _ h _ _) -> (h, (Sc_Func i A_InvalidType []))
                    (TypeSyntax     _ h _) -> (h, (Sc_Type i A_InvalidType  0))
                    (TyClassSyntax  _ h _) -> (h, (Sc_TyCs i A_InvalidType  0)))

      scobjs  :: [(Text, ScopeObj)]
      scobjs = L.map (\(i,d) -> modeldef (fromIntegral i, d)) $ M.assocs dfs

      filemap :: M.Map Text Int
      filemap = M.fromList $ L.zip (L.sort $ L.nub $ L.map fst scobjs) [2..]

      invfmap :: M.Map Int Text
      invfmap = M.fromList $ L.map Tp.swap $ M.assocs filemap

      scfpairs:: [(Text, [ScopeObj])]
      scfpairs= L.map (\xs -> (fst $ L.head xs, L.map snd xs)) $ L.groupBy groupair $ L.sortBy compair scobjs

      oscnames:: [(Text, Text, Int64)]
      oscnames= L.map (\(i,d) -> (hostfile d, identifier d, i)) $ M.assocs dfs

      fscnames:: [(Text, [(Text, Int64)])]
      fscnames= L.map (\xs->  (fst3 $ L.head xs, L.map dfst3 xs) ) $
                    L.groupBy groutrip $
                    L.sortBy  comtrip oscnames

      iscnames:: [(Text, [(Text, [Int])])]
      iscnames= L.map (\(f,xs) -> (f,
                    L.map (\xs-> (fst $ L.head xs, L.map (fromIntegral . snd) xs)) $
                    L.groupBy groupair $
                    L.sortBy compair xs) ) fscnames

      oscfmap :: M.Map Text (M.Map Text [Int])
      oscfmap = M.fromList $ L.map (\(f,xs) -> (f, M.fromList xs)) iscnames

      impmap  :: M.Map Text [(Text, [Int])]
      impmap  = M.fromList $ L.map (\(k,v)->(invfmap M.! k, v)) $ M.assocs $ getImports filemap dt

      scopes  :: [Scope]
      scopes  = L.map (\(x, scs) -> Scope (M.fromList $ L.zip [1..] scs) (oscfmap M.! x) (impmap M.! x)) scfpairs

      scopemap:: M.Map Int Scope
      scopemap= M.fromList $ L.zip [2..] scopes

  in (ScopeTable scopemap (M.size scopemap), filemap)
  where
        compair :: Ord a => (a, b) -> (a, b) -> Ordering
        compair (a,_) (b,_) = compare a b

        comtrip :: Ord a => (a, b, c) -> (a, b, c) -> Ordering
        comtrip (a,_,_) (b,_,_) = compare a b

        groupair:: Eq  a => (a, b) -> (a, b) -> Bool
        groupair(a,_) (b,_) = a == b

        groutrip:: Eq  a => (a, b, c) -> (a, b, c) -> Bool
        groutrip(a, _, _) (b, _, _) = a == b










{-
  BUG:
  It appears as though somehow missing files are getting through here?

  If a file import is missing, it will attempt to lookup and link the scopes.
  If the file has not been imported, it crashes.
-}
getImports :: M.Map Text Int -> DefinitionTable -> M.Map Int [(Text, [Int])]
getImports filemap dt@(DefinitionTable _ files _ _) =
  let
      fdata :: M.Map Int (Text, Text, Text)
      fdata =  M.fromList $ L.map (\(BzoFileModel mn fp dm _ _ _ _ _)-> (filemap M.! (pack fp), (pack fp, mn, dm))) files

      fincs :: M.Map Int (Text, [Text], [Text], [(Text, Text)], [(Text, Text)])
      fincs =  M.fromList $ L.map (\(BzoFileModel _ fp dm _ is ls ias las)-> (filemap M.! (pack fp), (dm, is, ls, ias, las))) files

      filespace :: M.Map Text [(Text, Int)]
      filespace = M.fromList $
                  L.map (\xs -> (fst $ L.head xs, L.map snd xs)) $
                  L.groupBy (\a b -> (fst a) == (fst b)) $
                  L.map (\(BzoFileModel mn fp dm _ _ _ _ _)-> (mn, (dm, filemap M.! (pack fp)))) files

      {-
        This is interesting; building a function that does lookup on a data
        structure built in local scope. I'm wondering if there's some kind of
        strange optimization that could be done on cases like this in a future
        version of Bzo.
      -}
      findImport :: Text -> Text -> [Int]
      findImport dm mn =
        case (L.lookup mn $ filespace M.! dm) of
          Just x  -> [x]
          Nothing -> []

      findLink   :: Text -> [Int]
      findLink dm = L.map snd $
        case (M.lookup dm filespace) of
          Just x  ->  x
          Nothing -> []

      fimps :: M.Map Int [(Text, [Int])]
      fimps = M.map (\(dm, is, ls, ias, las)->
                  (L.map (\ i    -> (i, findImport dm i)) is )  ++
                  (L.map (\(i,a) -> (a, findImport dm i)) ias)  ++
                  (L.map (\ l    -> (l, findLink      l)) ls )  ++
                  (L.map (\(l,a) -> (a, findLink      l)) las)) fincs

  in fimps










{-
  I think I might have to include something here to replace "@" with a domain
    id. Seems like that might cause fewer problems upstream.
-}
makeDefScope :: ScopeTable -> M.Map Text Int -> Definition -> (ScopeTable, Int)
makeDefScope sctab@(ScopeTable scs top) ftab def =
  let
      host :: Text
      host = hostfile def

      hsid :: Int
      hsid = ftab M.! host

      scope:: Scope
      scope= Scope M.empty M.empty [(pack "@", [hsid])]

  in (ScopeTable (M.insert (top+1) scope scs) (top+1), top+1)