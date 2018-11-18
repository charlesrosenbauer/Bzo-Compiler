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
import Builtins
import DefinitionTable
import Control.Parallel.Strategies
import Debug.Trace










divideIntoDefs :: [BzoSyntax] -> [Definition]
divideIntoDefs [BzS_Calls _ cs] = divideIntoDefs $ L.reverse cs
divideIntoDefs [BzS_File  _ _ _ _ _ dfs] = divideIntoDefs $ L.reverse dfs
divideIntoDefs asts = L.foldl divideDefStep [] asts
  where divideDefStep :: [Definition] -> BzoSyntax -> [Definition]
        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FunDef p _ fnid' _ _) =
          if fnid == fnid'
            then ((FuncSyntax fnid   file fty (fd:fdfs)):defs)
            else ((FuncSyntax fnid' (fileName p) (BzS_Undefined p) [fd]):f:defs)

        divideDefStep (f@(FuncSyntax fnid file fty fdfs):defs) fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax fnid' (fileName p) fd []):f:defs)

        divideDefStep defs fd@(BzS_FnTypeDef p _ fnid' _) =
          ((FuncSyntax fnid' (fileName p) fd []):defs)

        divideDefStep defs td@(BzS_TypDef p _ tyid _) =
          ((TypeSyntax tyid (fileName p) td):defs)

        divideDefStep defs td@(BzS_TyClassDef p _ tyid _) =
          ((TyClassSyntax tyid (fileName p) td):defs)

        divideDefStep defs fd@(BzS_FunDef p _ fnid' _ _) =
          ((FuncSyntax fnid' (fileName p) (BzS_Undefined p) [fd]):defs)










getFnIds :: [Definition] -> [Text]
getFnIds ((FuncDef    fnid _ _ _):defs) = (fnid):(getFnIds defs)
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
      alldefs = L.zip (L.map (\x->fromIntegral$topBuiltin+x) [0..]) $ L.concatMap (L.reverse . bfm_fileModel) files
      defmap :: Map Int64 Definition
      defmap  = M.fromList alldefs

      fileIxs = L.map swap alldefs
      idlists :: [[(Text, Int64)]]
      idlists = L.groupBy (\(a,_) (b,_) -> a == b) $ L.map (\(a, b) -> (identifier a, b)) fileIxs
      idmap :: Map Text [Int64]
      idmap   = M.fromList $ L.map (\xs -> (fst $ L.head xs, L.map (fromIntegral . snd) xs)) $ idlists

      ctranges= L.zip defCts (L.tail defCts)
      ctspaces :: [[Int64]]
      ctspaces= L.map (\(a, b) -> L.map (\x -> fromIntegral $ x+a) $ L.take (b-a) $ L.map (topBuiltin +) [0..]) ctranges
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
extractEnum ps (BzS_LispCall   _ fn expr) = (extractEnum ps fn)  ++ (L.concatMap (extractEnum ps) expr)
extractEnum ps (BzS_Poly       _    expr) = L.concatMap (enumOp ps) expr
  where enumOp :: BzoSyntax -> BzoSyntax -> [BzoSyntax]
        enumOp ps (BzS_Expr _ [BzS_FilterObj p (BzS_TyId _ t) [tdef]]) = (BzS_TypDef p ps t tdef):(extractEnum ps tdef)
        enumOp ps             (BzS_FilterObj p (BzS_TyId _ t) [tdef] ) = (BzS_TypDef p ps t tdef):(extractEnum ps tdef)
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
replaceEnum ps (BzS_LispCall   p fn expr) = (BzS_LispCall  p (replaceEnum ps fn)  (L.map (replaceEnum ps) expr))
replaceEnum ps (BzS_Poly       p    expr) = (BzS_Poly   p (L.map (enumOp ps) expr))
  where enumOp :: BzoSyntax -> BzoSyntax -> BzoSyntax
        enumOp (BzS_Undefined _) (BzS_FilterObj p (BzS_TyId _ t) [tdef]) =  BzS_TyId p t
        enumOp ps                (BzS_FilterObj p (BzS_TyId _ t) [tdef]) = (BzS_Expr p ((BzS_TyId p t):[ps]))
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
extractRecord ps tid depth (BzS_LispCall   _  fn expr) = (extractRecord ps tid depth  fn) ++ (L.concatMap (extractRecord ps tid depth) expr)
extractRecord ps tid depth (BzS_Cmpd       _     expr) = L.concatMap (\(d, xpr) -> recordOp ps tid d xpr) $ L.zip (addDepth depth expr) expr
  where recordOp :: BzoSyntax -> Text -> [(Int, Int)] -> BzoSyntax -> [BzoSyntax]
        recordOp (BzS_Undefined _) tid depth (BzS_FilterObj p (BzS_Id _ rcid) [tdef]) =
          [(BzS_FunDef    p (makeDepthPattern p depth) rcid (BzS_Undefined p) (BzS_Id p $ pack "x")),
           (BzS_FnTypeDef p (BzS_Undefined p) rcid (BzS_FnTy p (BzS_TyId p tid    )      tdef))]


        recordOp ps            tid depth (BzS_FilterObj p (BzS_Id _ rcid) [tdef]) =
          [(BzS_FunDef    p (makeDepthPattern p depth) rcid (BzS_Undefined p) (BzS_Id p $ pack "x")),
           (BzS_FnTypeDef p (BzS_Undefined p) rcid (BzS_FnTy p (BzS_Expr p [BzS_TyId p tid, ps])      tdef))]

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
replaceRecord ps tid depth (BzS_LispCall   p fn expr) = (BzS_LispCall  p (replaceRecord ps tid depth fn) (L.map (replaceRecord ps tid depth) expr))
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
removeBoxes (BzS_Lambda     p pars def) = (BzS_Lambda    p (removeBoxes pars) (removeBoxes def))
removeBoxes (BzS_FilterObj  p obj  fs ) = (BzS_FilterObj p (removeBoxes obj) (L.map removeBoxes fs))
removeBoxes (BzS_CurryObj   p obj prs ) = (BzS_CurryObj  p (removeBoxes obj) (L.map removeBoxes prs))
removeBoxes (BzS_LispCall   p fn  expr) = (BzS_LispCall  p (removeBoxes  fn) (L.map removeBoxes expr))
removeBoxes x = x










-- | fn  is applied to each subexpression
-- | swx is applied to each subexpression to determine if the current pass should be continued on said
-- |   subexpression. If not, the function it provides is applied instead.
-- | All errors produced from these passes are concatenated into a list and returned.
verifyAST :: (BzoSyntax -> [BzoErr]) -> (BzoSyntax -> Maybe (BzoSyntax -> [BzoErr])) -> BzoSyntax -> [BzoErr]
verifyAST fn swx x =
  let verify = verifyAST fn swx
  in case swx x of
      Just tx -> tx x
      Nothing ->
        case x of
          (BzS_Expr       p  expr) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] expr
          (BzS_Statement  p  expr) -> (fn x) ++ (fn expr)
          (BzS_Cmpd       p  expr) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] expr
          (BzS_Poly       p  expr) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] expr
          (BzS_Block      p  expr) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] expr
          (BzS_Calls      p calls) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] calls
          (BzS_FnTy       p i   o) -> (fn x) ++ (verify i) ++ (verify o)
          (BzS_TypDef     p i t o) -> (fn x) ++ (verify i) ++ (verify o)
          (BzS_FnTypeDef  p i f d) -> (fn x) ++ (verify i) ++ (verify d)
          (BzS_FunDef   p i f o d) -> (fn x) ++ (verify i) ++ (verify o) ++ (verify d)
          (BzS_TyClassDef p i c d) -> (fn x) ++ (verify i) ++ L.foldl (\a b -> a ++ (verify b)) [] d
          (BzS_ArrayObj   p _   t) -> (fn x) ++ (verify t)
          (BzS_Lambda     p i   d) -> (fn x) ++ (verify i) ++ (verify d)
          (BzS_FilterObj  p o   t) -> (fn x) ++ (verify o) ++ L.foldl (\a b -> a ++ (verify b)) [] t
          (BzS_CurryObj   p o   i) -> (fn x) ++ L.foldl (\a b -> a ++ (verify b)) [] i
          (BzS_LispCall   p f xpr) -> (fn x) ++ (verify f) ++ L.foldl (\a b -> a ++ (verify b)) [] xpr
          x                        ->  fn x










makeSntxErr :: Text -> BzoSyntax -> [BzoErr]
makeSntxErr txt syn = [SntxErr (pos syn) txt]










placeholderSwitch :: BzoSyntax -> Maybe (BzoSyntax -> [BzoErr])
placeholderSwitch _ = Nothing










validatePattern :: BzoSyntax -> [BzoErr]
validatePattern (BzS_Undefined _) = []
validatePattern patt = verifyAST verifyPattern switchPattern patt

verifyPattern :: BzoSyntax -> [BzoErr]
verifyPattern (BzS_Lambda       p _ _) = [SntxErr p $ pack "Unexpected Lambda expression in pattern"]
verifyPattern (BzS_TyClassDef p _ _ _) = [SntxErr p $ pack "Unexpected Function Type Definition in pattern"]
verifyPattern (BzS_FnTypeDef  p _ _ _) = [SntxErr p $ pack "Unexpected Function Type Definition in pattern"]
verifyPattern (BzS_Calls          p _) = [SntxErr p $ pack "Unexpected Definitions in pattern"]
verifyPattern (BzS_Block          p _) = [SntxErr p $ pack "Unexpected Block Expression in pattern"]
verifyPattern (BzS_TypDef     p _ _ _) = [SntxErr p $ pack "Unexpected Type Definition in pattern"]
verifyPattern (BzS_FunDef   p _ _ _ _) = [SntxErr p $ pack "Unexpected Function Definition in pattern"]
verifyPattern (BzS_Import       p _ _) = [SntxErr p $ pack "Unexpected File Import in pattern"]
verifyPattern (BzS_Include      p _ _) = [SntxErr p $ pack "Unexpected File Inclusion in pattern"]
verifyPattern (BzS_File   p _ _ _ _ _) = [SntxErr p $ pack "Unexpected File Definition in pattern"]
verifyPattern (BzS_MapObj         p _) = [SntxErr p $ pack "Unexpected Map expression in pattern"]
verifyPattern _ = []

switchPattern :: BzoSyntax -> Maybe (BzoSyntax -> [BzoErr])
switchPattern (BzS_FilterObj _ o filts) = Just (\_ -> (validateExpr   o) ++ (L.concatMap validateType filts))
switchPattern _ = Nothing










validateType :: BzoSyntax -> [BzoErr]
validateType ty = verifyAST verifyType switchType ty

verifyType :: BzoSyntax -> [BzoErr]
verifyType (BzS_MId            p _) = [SntxErr p $ pack "Unexpected Mutable Variable in type"]
verifyType (BzS_Lambda       p _ _) = [SntxErr p $ pack "Unexpected Lambda Expression in type"]
verifyType (BzS_FnTypeDef  p _ _ _) = [SntxErr p $ pack "Unexpected Function Type Definition in type"]
verifyType (BzS_TyClassDef p _ _ _) = [SntxErr p $ pack "Unexpected Type Class Definition in type"]
verifyType (BzS_Calls          p _) = [SntxErr p $ pack "Unexpected Calls in type"]
verifyType (BzS_Block          p _) = [SntxErr p $ pack "Unexpected Block Expression in type"]
verifyType (BzS_TypDef     p _ _ _) = [SntxErr p $ pack "Unexpected Type Definition in type"]
verifyType (BzS_FunDef   p _ _ _ _) = [SntxErr p $ pack "Unexpected Function Definition in type"]
verifyType (BzS_Import       p _ _) = [SntxErr p $ pack "Unexpected File Import in type"]
verifyType (BzS_Include      p _ _) = [SntxErr p $ pack "Unexpected File Inclusion in type"]
verifyType (BzS_File   p _ _ _ _ _) = [SntxErr p $ pack "Unexpected File Definition in type"]
verifyType (BzS_MapObj         p _) = [SntxErr p $ pack "Unexpected Map expression in type"]
verifyType (BzS_Wildcard         p) = [SntxErr p $ pack "Unexpected Wildcard in type"]
verifyType _ = []

switchType :: BzoSyntax -> Maybe (BzoSyntax -> [BzoErr])
switchType _ = Nothing










validateExpr :: BzoSyntax -> [BzoErr]
validateExpr expr = verifyAST verifyExpr switchExpr expr

verifyExpr :: BzoSyntax -> [BzoErr]
verifyExpr (BzS_TyClassDef p _ _ _) = [SntxErr p $ pack "Unexpected type class definition inside expression"]
verifyExpr (BzS_FnTypeDef  p _ _ _) = [SntxErr p $ pack "Unexpected function type definition inside expression"]
verifyExpr (BzS_Calls          p _) = [SntxErr p $ pack "Unexpected definitions inside expression"]
verifyExpr (BzS_TypDef     p _ _ _) = [SntxErr p $ pack "Unexpected type definition inside expression"]
verifyExpr (BzS_FunDef   p _ _ _ _) = [SntxErr p $ pack "Unexpected function definition inside expression"]
verifyExpr (BzS_Import       p _ _) = [SntxErr p $ pack "Unexpected file import inside expression"]
verifyExpr (BzS_Include      p _ _) = [SntxErr p $ pack "Unexpected file inclusion inside expression"]
verifyExpr (BzS_File   p _ _ _ _ _) = [SntxErr p $ pack "Unexpected file definition inside expression"]
verifyExpr _ = []

switchExpr :: BzoSyntax -> Maybe (BzoSyntax -> [BzoErr])
switchExpr (BzS_FilterObj _ o filts) = Just (\_ -> (validateExpr   o) ++ (L.concatMap validateType filts))
switchExpr (BzS_Lambda    _ ps  def) = Just (\_ -> (validateExpr def) ++ (validatePattern ps))
switchExpr _ = Nothing










-- I don't think I need any switches here or anything
verifyTyPattern :: BzoSyntax -> [BzoErr]
verifyTyPattern (BzS_Undefined   _) = []
verifyTyPattern (BzS_Cmpd p     xs) = L.concatMap isValidPar xs
verifyTyPattern (BzS_Expr p [expr]) = isValidPar expr
verifyTyPattern x                   = isValidPar x


isValidPar :: BzoSyntax -> [BzoErr]
isValidPar (BzS_Expr  p [x]) = isValidPar x
isValidPar (BzS_TyVar p  _ ) = []
isValidPar (BzS_FilterObj p (BzS_TyVar _ v) filts) = L.concatMap validateType filts
isValidPar x = [SntxErr (pos x) $ pack $ "Expected a type variable, or filtered type variable." ++ (show x)]











verifyCall :: BzoSyntax -> [BzoErr]
verifyCall (BzS_TyClassDef _ ps _ df) = L.foldl (\a b -> a ++ (verifyCall b)) (verifyTyPattern ps) df
verifyCall (BzS_FnTypeDef  _ ps _ df) = (validateType df) ++ (verifyTyPattern ps)
verifyCall (BzS_TypDef     _ ps _ df) = (validateType df) ++ (verifyTyPattern ps)
verifyCall (BzS_FunDef  _ is _ xs df) = (validatePattern is) ++ (validatePattern xs) ++ (validateExpr df)
verifyCall (BzS_Calls   _        dfs) = L.foldl (\a b -> a ++ (verifyCall b)) [] dfs
verifyCall x = [SntxErr (pos x) $ pack "Expected a definition, found something else"]










wrappedVerifier :: [BzoFileModel BzoSyntax] -> Either [BzoErr] [BzoFileModel BzoSyntax]
wrappedVerifier files =
  case L.concatMap (verifyCall . bfm_fileModel) files of
    [] -> Right files
    er -> Left  er










modelXForm :: BzoSyntax -> BzoSyntax
modelXForm (BzS_Calls p ast) =
  let ast' = L.map removeBoxes ast
      enms = L.concatMap (extractEnum   (BzS_Undefined p)             ) ast'
      rcds = L.concatMap (extractRecord (BzS_Undefined p) (pack "") []) ast'

      allAST  = rcds ++ enms ++ ast'
      allAST' = parMap rpar ((replaceRecord (BzS_Undefined p) (pack "") []) . (replaceEnum (BzS_Undefined p))) allAST
  in (BzS_Calls p allAST')
