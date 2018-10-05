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
getTVars (BzS_TyVar      _    tvar) = [tvar]
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
getTVars _                          = []










getVars :: BzoSyntax -> [Text]
getVars (BzS_Id         _     var) = [var]
getVars (BzS_MId        _     var) = [var]
getVars (BzS_BId        _     var) = [var]
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
getVars _                          = []










getTypes :: BzoSyntax -> [Text]
getTypes (BzS_TyId       _     var) = [var]
getTypes (BzS_BTId       _     var) = [var]
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
getTypes _                          = []










divideIntoDefs :: [BzoSyntax] -> [Definition]
divideIntoDefs [BzS_Calls _ cs] = divideIntoDefs $ L.reverse cs
divideIntoDefs [BzS_File  _ _ _ _ _ dfs] = divideIntoDefs $ L.reverse dfs
divideIntoDefs asts = L.foldl divideDefStep [] asts
  where divideDefStep :: [Definition] -> BzoSyntax -> [Definition]
        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FunDef p _ fnid' _ _) =
          if fnid == fnid'
            then ((FuncSyntax fnid   file fty (fd:fdfs)):defs)
            else ((FuncSyntax fnid' (fileName p) BzS_Undefined [fd]):f:defs)

        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax fnid' (fileName p) fd []):f:defs)

        divideDefStep defs fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax fnid' (fileName p) fd []):defs)

        divideDefStep defs td@(BzS_TypDef p _ tyid _) =
          ((TypeSyntax tyid (fileName p) td):defs)

        divideDefStep defs td@(BzS_TyClassDef p _ tyid _) =
          ((TyClassSyntax tyid (fileName p) td):defs)

        divideDefStep defs fd@(BzS_FunDef p _ fnid' _ _) =
          ((FuncSyntax fnid' (fileName p) BzS_Undefined [fd]):defs)










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










getDefTable :: [BzoFileModel [Definition]] -> DefinitionTable
getDefTable files =
  let defCts  = L.scanl (+) 0 $ L.map (L.length . bfm_fileModel) files

      alldefs :: [(Int64, Definition)]
      alldefs = L.zip [0..] $ L.concatMap (L.reverse . bfm_fileModel) files
      defmap :: Map Int64 Definition
      defmap  = M.fromList alldefs

      fileIxs = L.map swap alldefs
      idlists :: [[(Text, Int64)]]
      idlists = L.groupBy (\(a,_) (b,_) -> a == b) $ L.map (\(a, b) -> (identifier a, b)) fileIxs
      idmap :: Map Text [Int64]
      idmap   = M.fromList $ L.map (\xs -> (fst $ L.head xs, L.map (fromIntegral . snd) xs)) $ idlists

      ctranges= L.zip defCts (L.tail defCts)
      ctspaces :: [[Int64]]
      ctspaces= L.map (\(a, b) -> L.map (\x -> fromIntegral $ x+a) $ L.take (b-a) [0..]) ctranges
      filelist= L.map (\(space, f)-> replaceModel f space) $ L.zip ctspaces files


      dmfiles  :: [(Text, [Int64])]
      dmfiles = L.map (\xs -> (bfm_domain $ L.head xs, L.concatMap bfm_fileModel xs)) $ L.groupBy (\a b-> (bfm_domain a) == (bfm_domain b)) filelist
      dmspaces= M.fromList dmfiles

      mdfiles  :: [((Text, Text), [Int64])]
      mdfiles = L.map (\xs -> ((bfm_moduleName $ L.head xs, bfm_domain $ L.head xs), L.concatMap bfm_fileModel xs)) $ L.groupBy (\a b-> (bfm_filepath a) == (bfm_filepath b)) filelist
      mdspaces= M.fromList mdfiles


      filelist' = L.map (\bfm-> let imps = (bfm_fileImports bfm) ++ (L.map fst $ bfm_fileImportsAs bfm)
                                    lnks = (bfm_fileLinks   bfm) ++ (L.map fst $ bfm_fileLinksAs   bfm)
                                    domn = bfm_domain bfm
                                    idefs= catMaybes $ L.map (\k -> M.lookup (k, domn) mdspaces) imps
                                    ldefs= catMaybes $ L.map (\k -> M.lookup  k        dmspaces) lnks
                                    model= (bfm_fileModel bfm, L.nub $ L.concat (idefs ++ ldefs ++ [bfm_fileModel bfm]))
                                in  replaceModel bfm model) filelist

  in (DefinitionTable defmap filelist' idmap (fromIntegral $ L.last defCts))








-- This doesn't replace lambdas in the AST with anything new
extractLambda :: BzoSyntax -> [BzoSyntax]
extractLambda (BzS_Expr       _    expr) = L.concatMap extractLambda expr
extractLambda (BzS_Statement  _    expr) = extractLambda expr
extractLambda (BzS_Cmpd       _    expr) = L.concatMap extractLambda expr
extractLambda (BzS_Poly       _    expr) = L.concatMap extractLambda expr
extractLambda (BzS_Block      _    expr) = L.concatMap extractLambda expr
extractLambda (BzS_FunDef     _ _ _ _ x) = (extractLambda x)
extractLambda (BzS_Calls      _      cs) = L.concatMap extractLambda cs
extractLambda (BzS_ArrayObj   _  _ expr) = extractLambda expr
extractLambda (BzS_FilterObj  _ obj  fs) = (extractLambda obj) ++ (L.concatMap extractLambda fs)
extractLambda (BzS_CurryObj   _ obj  ps) = (extractLambda obj) ++ (L.concatMap extractLambda ps)
extractLambda (BzS_MapObj     _    expr) = (extractLambda expr)
extractLambda (BzS_Lambda     p ps expr) = [BzS_FunDef p ps (pack $ show p) BzS_Undefined expr] ++ (extractLambda expr)
extractLambda _                          = []










-- This does replace lambdas in the AST
replaceLambda :: BzoSyntax -> BzoSyntax
replaceLambda (BzS_Expr       p    expr) = (BzS_Expr   p (L.map replaceLambda expr))
replaceLambda (BzS_Statement  p    expr) = (BzS_Statement p (replaceLambda expr))
replaceLambda (BzS_Cmpd       p    expr) = (BzS_Cmpd   p (L.map replaceLambda expr))
replaceLambda (BzS_Poly       p    expr) = (BzS_Poly   p (L.map replaceLambda expr))
replaceLambda (BzS_Block      p    expr) = (BzS_Block  p (L.map replaceLambda expr))
replaceLambda (BzS_FunDef     p i f o x) = (BzS_FunDef p i f o (replaceLambda x))
replaceLambda (BzS_Calls      p      cs) = (BzS_Calls  p (L.map replaceLambda cs))
replaceLambda (BzS_ArrayObj   p a  expr) = (BzS_ArrayObj  p a (replaceLambda expr))
replaceLambda (BzS_FilterObj  p obj  fs) = (BzS_FilterObj p (replaceLambda obj) (L.map replaceLambda fs))
replaceLambda (BzS_CurryObj   p obj  ps) = (BzS_CurryObj  p (replaceLambda obj) (L.map replaceLambda ps))
replaceLambda (BzS_MapObj     p    expr) = (BzS_MapObj p (replaceLambda expr))
replaceLambda (BzS_Lambda     p ps expr) = (BzS_Id p (pack $ show p))
replaceLambda x                          = x










-- This doesn't replace Enums in the AST with anything new
extractEnum :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
extractEnum ps (BzS_Expr       _    expr) = L.concatMap (extractEnum ps) expr
extractEnum ps (BzS_Statement  _    expr) = extractEnum ps expr
extractEnum ps (BzS_Cmpd       _    expr) = L.concatMap (extractEnum ps) expr
extractEnum ps (BzS_Block      _    expr) = L.concatMap (extractEnum ps) expr
extractEnum _  (BzS_TypDef     _ ps  _ x) = (extractEnum ps x)
extractEnum ps (BzS_Calls      _      cs) = L.concatMap (extractEnum ps) cs
extractEnum ps (BzS_ArrayObj   _  _ expr) = extractEnum ps expr
extractEnum ps (BzS_FilterObj  _ obj  fs) = (extractEnum ps obj) ++ (L.concatMap (extractEnum ps) fs)
extractEnum ps (BzS_CurryObj   _ obj prs) = (extractEnum ps obj) ++ (L.concatMap (extractEnum ps) prs)
extractEnum ps (BzS_Poly       _    expr) = L.concatMap (enumOp ps) expr
  where enumOp :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
        enumOp ps (BzS_Expr _ [BzS_FilterObj p (BzS_TyId _ t) [tdef]]) = (BzS_TypDef p ps t tdef):(extractEnum ps tdef)
        enumOp ps x = extractEnum ps x
extractEnum _  _                          = []










-- This does replace Enums in the AST
replaceEnum :: BzoSyntax -> BzoSyntax -> BzoSyntax
replaceEnum ps (BzS_Expr       p    expr) = (BzS_Expr   p (L.map (replaceEnum ps) expr))
replaceEnum ps (BzS_Statement  p    expr) = (BzS_Statement p (replaceEnum ps expr))
replaceEnum ps (BzS_Cmpd       p    expr) = (BzS_Cmpd   p (L.map (replaceEnum ps) expr))
replaceEnum ps (BzS_Block      p    expr) = (BzS_Block  p (L.map (replaceEnum ps) expr))
replaceEnum _  (BzS_TypDef     p i  t  x) = (BzS_TypDef p i t (replaceEnum i x))
replaceEnum ps (BzS_Calls      p      cs) = (BzS_Calls  p (L.map (replaceEnum ps) cs))
replaceEnum ps (BzS_ArrayObj   p a  expr) = (BzS_ArrayObj  p a (replaceEnum ps expr))
replaceEnum ps (BzS_FilterObj  p obj  fs) = (BzS_FilterObj p (replaceEnum ps obj) (L.map (replaceEnum ps) fs))
replaceEnum ps (BzS_CurryObj   p obj prs) = (BzS_CurryObj  p (replaceEnum ps obj) (L.map (replaceEnum ps) prs))
replaceEnum ps (BzS_Poly       p    expr) = (BzS_Poly   p (L.map (enumOp ps) expr))
  where enumOp :: BzoSyntax -> BzoSyntax -> BzoSyntax
        enumOp BzS_Undefined (BzS_FilterObj p (BzS_TyId _ t) [tdef]) =  BzS_TyId p t
        enumOp ps            (BzS_FilterObj p (BzS_TyId _ t) [tdef]) = (BzS_Expr p ((BzS_TyId p t):[ps]))
        enumOp ps x = replaceEnum ps x
replaceEnum _  x                          = x




















-- This doesn't replace Records in the AST with anything new
extractRecord :: BzoSyntax -> Text -> [(Int, Int)] -> BzoSyntax -> [BzoSyntax]
extractRecord ps tid depth (BzS_Expr       _     expr) = L.concatMap (extractRecord ps tid depth) expr
extractRecord ps tid depth (BzS_Statement  _     expr) = extractRecord ps tid depth expr
extractRecord ps tid depth (BzS_Poly       _     expr) = L.concatMap (extractRecord ps tid depth) expr
extractRecord ps tid depth (BzS_Block      _     expr) = L.concatMap (extractRecord ps tid depth) expr
extractRecord _  _   depth (BzS_TypDef     _ ps tid x) = (extractRecord ps tid depth x)
extractRecord ps tid depth (BzS_Calls      _       cs) = L.concatMap (extractRecord ps tid depth) cs
extractRecord ps tid depth (BzS_ArrayObj   _  _  expr) = extractRecord ps tid depth expr
extractRecord ps tid depth (BzS_FilterObj  _  obj  fs) = (extractRecord ps tid depth obj) ++ (L.concatMap (extractRecord ps tid depth) fs)
extractRecord ps tid depth (BzS_CurryObj   _  obj prs) = (extractRecord ps tid depth obj) ++ (L.concatMap (extractRecord ps tid depth) prs)
extractRecord ps tid depth (BzS_Cmpd       _     expr) = L.concatMap (\(d, xpr) -> recordOp ps tid d xpr) $ L.zip (addDepth depth expr) expr
  where recordOp :: BzoSyntax -> Text -> [(Int, Int)] -> BzoSyntax -> [BzoSyntax]
        recordOp BzS_Undefined tid depth (BzS_FilterObj p (BzS_Id _ rcid) [tdef]) =
          [(BzS_FunDef    p (makeDepthPattern p depth) rcid BzS_Undefined (BzS_Id p $ pack "x")),
           (BzS_FnTypeDef p BzS_Undefined rcid (BzS_FnTy p (BzS_TyId p tid    )      tdef))]


        recordOp ps            tid depth (BzS_FilterObj p (BzS_Id _ rcid) [tdef]) =
          [(BzS_FunDef    p (makeDepthPattern p depth) rcid BzS_Undefined (BzS_Id p $ pack "x")),
           (BzS_FnTypeDef p BzS_Undefined rcid (BzS_FnTy p (BzS_Expr p [BzS_TyId p tid, ps])      tdef))]

        recordOp ps tid depth x = extractRecord ps tid depth x
extractRecord _ _ _ _                                 = []










-- This does replace Records in the AST
replaceRecord :: BzoSyntax -> Text -> [(Int, Int)] -> BzoSyntax -> BzoSyntax
replaceRecord ps tid depth (BzS_Expr       p    expr) = (BzS_Expr   p (L.map (replaceRecord ps tid depth) expr))
replaceRecord ps tid depth (BzS_Statement  p    expr) = (BzS_Statement p (replaceRecord ps tid depth expr))
replaceRecord ps tid depth (BzS_Poly       p    expr) = (BzS_Poly   p (L.map (replaceRecord ps tid depth) expr))
replaceRecord ps tid depth (BzS_Block      p    expr) = (BzS_Block  p (L.map (replaceRecord ps tid depth) expr))
replaceRecord _  _   depth (BzS_TypDef     p i tid x) = (BzS_TypDef p i tid (replaceRecord i tid depth x))
replaceRecord ps tid depth (BzS_Calls      p      cs) = (BzS_Calls  p (L.map (replaceRecord ps tid depth) cs))
replaceRecord ps tid depth (BzS_ArrayObj   p a  expr) = (BzS_ArrayObj  p a (replaceRecord ps tid depth expr))
replaceRecord ps tid depth (BzS_FilterObj  p obj  fs) = (BzS_FilterObj p (replaceRecord ps tid depth obj) (L.map (replaceRecord ps tid depth) fs))
replaceRecord ps tid depth (BzS_CurryObj   p obj prs) = (BzS_CurryObj  p (replaceRecord ps tid depth obj) (L.map (replaceRecord ps tid depth) prs))
replaceRecord ps tid depth (BzS_Cmpd       p    expr) = (BzS_Cmpd   p (L.map (\(d, xpr) -> recordOp  ps tid d xpr) $ L.zip (addDepth depth expr) expr))
  where recordOp :: BzoSyntax -> Text -> [(Int, Int)] -> BzoSyntax -> BzoSyntax
        recordOp _  tid depth (BzS_FilterObj p (BzS_Id _ rcid) [tdef]) = tdef

        recordOp ps tid depth x = replaceRecord ps tid depth x
replaceRecord _ _ _  x                                = x










addDepth :: [(Int, Int)] -> [a] -> [[(Int, Int)]]
addDepth ds xs =
  let dss = L.repeat ds
      len = L.length xs
      xs' = L.take len $ L.zip (L.repeat len) [0..]
  in L.map (\(d, ds) -> ds ++ [d]) $ L.zip xs' dss










makeDepthPattern :: BzoPos -> [(Int, Int)] -> BzoSyntax
makeDepthPattern p [] = BzS_Id p $ pack "x"
makeDepthPattern p ((l, x):xs) = (BzS_Cmpd p $ L.take l (L.map (depthStage p x xs) [0..]))
  where depthStage :: BzoPos -> Int -> [(Int, Int)] -> Int -> BzoSyntax
        depthStage p x xs n =
          if x == n
            then makeDepthPattern p xs
            else BzS_Wildcard p










removeBoxes :: BzoSyntax -> BzoSyntax
removeBoxes (BzS_Expr       p   [expr]) = removeBoxes expr
removeBoxes (BzS_Expr       p    expr ) = (BzS_Expr      p (L.map removeBoxes expr))
removeBoxes (BzS_Statement  p    expr ) = (BzS_Statement p (removeBoxes expr))
removeBoxes (BzS_Cmpd       p   [expr]) = removeBoxes expr
removeBoxes (BzS_Poly       p   [expr]) = removeBoxes expr
removeBoxes (BzS_Cmpd       p    expr ) = (BzS_Cmpd      p (L.map removeBoxes expr))
removeBoxes (BzS_Poly       p    expr ) = (BzS_Poly      p (L.map removeBoxes expr))
removeBoxes (BzS_Block      p    expr ) = (BzS_Block     p (L.map removeBoxes expr))
removeBoxes (BzS_TypDef     p i  t  x ) = (BzS_TypDef    p i t (removeBoxes x))
removeBoxes (BzS_Calls      p      cs ) = (BzS_Calls     p (L.map removeBoxes cs))
removeBoxes (BzS_ArrayObj   p  a  expr) = (BzS_ArrayObj  p a (removeBoxes expr))
removeBoxes (BzS_FilterObj  p obj  fs ) = (BzS_FilterObj p (removeBoxes obj) (L.map removeBoxes fs))
removeBoxes (BzS_CurryObj   p obj prs ) = (BzS_CurryObj  p (removeBoxes obj) (L.map removeBoxes prs))
removeBoxes x = x








modelXForm :: BzoSyntax -> BzoSyntax
modelXForm (BzS_Calls p ast) =
  let ast' = L.map removeBoxes ast
      lams = L.concatMap  extractLambda ast
      enms = L.concatMap (extractEnum   BzS_Undefined             ) ast'
      rcds = L.concatMap (extractRecord BzS_Undefined (pack "") []) ast'

      allAST  = rcds ++ enms ++ lams ++ ast'
      allAST' = L.map ((replaceRecord BzS_Undefined (pack "") []) . (replaceEnum BzS_Undefined) . replaceLambda) allAST
  in (BzS_Calls p allAST')
