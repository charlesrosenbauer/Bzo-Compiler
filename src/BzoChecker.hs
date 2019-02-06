module BzoChecker where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import Data.Either
import Query
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tuple as Tp
import Control.Parallel.Strategies
import Debug.Trace










noOverloadTypes :: DefinitionTable -> [BzoErr]
noOverloadTypes dt@(DefinitionTable defs files ids _) =
  let
      types :: [Definition]
      types  = L.filter isType $ M.elems defs

      nubbed:: [Definition]
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








{-

-- Takes type parameters and returns an associated header
initializeTypeHeader :: BzoSyntax -> TypeHeader
initializeTypeHeader (BzS_Undefined p) = TyHeader M.empty
initializeTypeHeader (BzS_Expr _ [x])  = initializeTypeHeader x

initializeTypeHeader (BzS_TyVar p v)        =
  TyHeader $ M.fromList [(1, TVrAtom p v        []                                           $ UnresType $ BzS_Undefined p)]

initializeTypeHeader (BzS_FilterObj p v fs) =
  TyHeader $ M.fromList [(1, TVrAtom p (sid v) (L.map (\t -> Constraint p $ UnresType t) fs) $ UnresType $ BzS_Undefined p)]

initializeTypeHeader (BzS_Cmpd _ vs) =
  let atoms = L.map makeAtom vs
  in  TyHeader $ M.fromList $ L.zip [1..] atoms
  where makeAtom :: BzoSyntax -> Atom
        makeAtom (BzS_TyVar     p v   ) =
          TVrAtom p      v  []                                            $ UnresType $ BzS_Undefined p
        makeAtom (BzS_FilterObj p v fs) =
          TVrAtom p (sid v) (L.map (\t -> Constraint p $ UnresType t) fs) $ UnresType $ BzS_Undefined p
        makeAtom (BzS_Expr _ [x]) = makeAtom x

initializeTypeHeader (BzS_FnTypeDef _ ps fn (BzS_FnTy _ i o)) =
  let tyhead = initializeTypeHeader ps

      tvars :: [(Text, BzoPos)]
      tvars  = (getTVars i) ++ (getTVars o)

      vnames :: S.Set Text
      vnames = S.fromList $ L.map fst tvars

      tvatms :: [Atom]
      tvatms = L.map (\(v,p) -> TVrAtom p v [] $ UnresType $ BzS_Undefined p) tvars
      tvatms'= L.nubBy (\(TVrAtom _ a _ _) (TVrAtom _ b _ _) -> a == b) $ L.filter (\(TVrAtom _ x _ _) -> S.member x vnames) tvatms

      key :: Int64
      key    = L.maximum $ [0] ++ (M.keys $ tvarmap tyhead)

      tvsold :: [(TVId, Atom)]
      tvsold = M.assocs $ tvarmap tyhead

      tvsnew :: [(TVId, Atom)]
      tvsnew = L.zip (L.map (key+) [1..]) tvatms'

      tvsall :: [(TVId, Atom)] -- I don't fully trust the nub here, but it seems to mostly work.
      tvsall = L.nubBy (\(_, (TVrAtom _ a _ _)) (_, (TVrAtom _ b _ _)) -> a == b) $ tvsold ++ tvsnew

  in TyHeader $ M.fromList tvsall










makeType :: SymbolTable -> TypeHeader -> BzoSyntax -> Either [BzoErr] Type
makeType st th (BzS_Expr   p  [x]) = makeType st th x
makeType st th (BzS_Statement p x) = makeType st th x
makeType st th (BzS_Cmpd  p  xs) = onAllPass (L.map (makeType st th) xs) (\ys -> CmpdType p ys)
makeType st th (BzS_Poly  p  xs) = onAllPass (L.map (makeType st th) xs) (\ys -> PolyType p ys)
makeType st th (BzS_Int   p   n) = Right (IntType  p n)
makeType st th (BzS_Flt   p   n) = Right (FltType  p n)
makeType st th (BzS_Str   p   s) = Right (StrType  p s)
makeType st th (BzS_Nil   p )    = Right (VoidType p)
makeType st th (BzS_BTId  p bit) = Right (BITyType p $ isBuiltinType bit)
makeType st th (BzS_ArrayObj p s x) = onAllPass [makeType st th x] (\[y] -> ArryType p s y)
makeType st th (BzS_FnTy  p i o) =
  let i' = makeType st th i
      o' = makeType st th o
      io = rights [i', o']
      er = lefts  [i', o']
  in case (er, io) of
      ([], [it, ot]) -> Right $ FuncType p it ot
      (er,       _ ) -> Left  $ L.concat er

makeType st th (BzS_Expr  p xs)  = onAllPass (L.map (makeType st th) xs) (\ys -> MakeType p ys)
makeType st@(SymbolTable _ _ vis) th ty@(BzS_TyId  p   t) =
  let ids = resolveGlobalId st ty
  in case ids of
      []  -> Left [TypeErr p $ pack ("Type " ++ (unpack t) ++ " is undefined.")]
      [x] -> Right (LtrlType p x)
      xs  -> Left [TypeErr p $ pack ("Ambiguous reference to type " ++ (unpack t) ++ ": " ++ (show xs) ++ " / " ++ (show $ M.assocs vis))]
makeType st@(SymbolTable _ _ vis) (TyHeader tvs) (BzS_TyVar p   v) =
  let tvpairs = M.assocs tvs
      tvnames = L.map (\(n,atm) -> (n, Mb.fromMaybe (pack "") $ atomId atm)) tvpairs
      ids = L.map fst $ L.filter (\(n,atm) -> v == atm) tvnames
  in case ids of
      []  -> Left [TypeErr p $ pack ("Type Variable " ++ (unpack v) ++ " is not defined in the type header.")]
      [x] -> Right (TVarType p x)
      xs  -> Left [TypeErr p $ pack ("Ambiguous reference to type variable " ++ (unpack v) ++ ": " ++ (show xs) ++ " / " ++ (show $ M.assocs vis))]

makeType st th (BzS_Id p f) =
  let fids = resolveGlobalId st (BzS_Id p f)
  in case fids of
      [] -> Left [TypeErr p $ pack $ "Function " ++ (unpack f) ++ " is not defined in this scope."]
      xs -> Right (FLitType p xs)

makeType st th ty@(BzS_Undefined _) = Right (UnresType ty)
makeType st th x = Left [TypeErr (pos x) $ pack $ "Malformed type expression: " ++ show x]










--getDefType :: Definition -> Either [BzoErr] Type
--getDefType ()











data ValNest
      = ValNest BzoPos [ValNest]
      | ValAtom BzoPos Int Type










modelFuncExpr :: SymbolTable -> BzoSyntax -> Either [BzoErr] (Pattern, Expr)
modelFuncExpr syms (BzS_FunDef p ips fnid xps def) =
  let ips' = (UnresPtrn ips)
      xps' = (UnresPtrn xps)
      def' = (UnresExpr (pos def) def)
      pat  = (ParamPtrn p ips' xps')
  in Right (pat, def')










modelDefs :: SymbolTable -> Definition -> Either [BzoErr] Definition
modelDefs syms (FuncSyntax fnid fname (BzS_Undefined p) fdefs) =
  let
      fndefs :: [Either [BzoErr] (Pattern, Expr)]
      fndefs = L.map (modelFuncExpr syms) fdefs   -- Change this when expression modelling exists

      fnerrs :: [BzoErr]
      fnerrs = L.concat $ lefts  fndefs

      fndefs':: [(Pattern, Expr)]
      fndefs'= rights fndefs

      tyhead :: TypeHeader
      tyhead = TyHeader M.empty

      fntype :: Type
      fntype = UnresType $ BzS_Expr p fdefs
  in case fnerrs of
      [] -> Right (FuncDef fnid fname tyhead fntype FuncPropEmpty fndefs')
      _  -> Left  fnerrs

modelDefs syms f@(FuncSyntax fnid fname ftyp@(BzS_FnTypeDef p ps _ tdef) fdefs) =
  let
      fndefs :: [Either [BzoErr] (Pattern, Expr)]
      fndefs = L.map (modelFuncExpr syms) fdefs   -- Change this when expression modelling exists

      fnerrs :: [BzoErr]
      fnerrs = L.concat $ lefts  fndefs

      fndefs':: [(Pattern, Expr)]
      fndefs'= rights fndefs

      tyhead :: TypeHeader
      tyhead = initializeTypeHeader ftyp

      fntype :: Either [BzoErr] Type
      fntype = {-Right $ UnresType tdef-} makeType syms tyhead tdef

      fntype':: Type
      fntype'= L.head $ (++ [InvalidType]) $ rights [fntype]

      errs   :: [BzoErr]
      errs   = L.concat $ lefts [fntype]
  in case (errs ++ fnerrs) of
      [] -> Right (FuncDef fnid fname tyhead fntype' FuncPropEmpty fndefs')
      er -> Left er

modelDefs syms t@(TypeSyntax tyid fname (BzS_TypDef p pars _ typ)) =
  let
      tyhead :: TypeHeader
      tyhead = initializeTypeHeader pars
      tydef  = {-Right $ UnresType typ ---}makeType syms tyhead typ

      tydef' :: Type
      tydef' = L.head $ (++ [InvalidType]) $ rights [tydef]

      errs   :: [BzoErr]
      errs   = L.concat $ lefts [tydef]
  in case errs of
      [] -> Right (TypeDef tyid fname tyhead TypePropEmpty tydef')
      er -> Left er

modelDefs syms (TyClassSyntax tcid fname (BzS_TyClassDef p pars _ defs)) =
  let
      tyhead :: TypeHeader
      tyhead = initializeTypeHeader pars

      -- tcdefs :: --INSERT-TYPE-HERE--
      tcdefs = []     -- Add this later. There'll be some complications with multiple type headers here.

      errs :: [BzoErr]
      errs   = []
  in case errs of
      [] -> Right (TyClassDef tcid fname tyhead TClsPropEmpty tcdefs)
      er -> Left er










modelProgram :: DefinitionTable -> Either [BzoErr] (DefinitionTable, M.Map Text SymbolTable)
modelProgram dt@(DefinitionTable defs files ids top) =
  let
      syms :: M.Map Text SymbolTable
      syms = M.fromList $ L.map (\f -> (pack $ bfm_filepath f, makeSymbolTable dt $ bfm_filepath f)) files

      defs' :: M.Map Int64 (Either [BzoErr] Definition)
      defs'= M.map (\d -> modelDefs (syms M.! (hostfile d)) d) defs

      ermp  :: M.Map Int64 [BzoErr]
      dfmp  :: M.Map Int64 Definition
      (ermp, dfmp) = sepEitherMaps defs'

      errs  :: [BzoErr]
      errs = L.concat $ M.elems ermp
  in case errs of
      [] -> Right (DefinitionTable dfmp files ids top, syms)
      er -> Left  er
-}










checkProgram :: DefinitionTable -> Either [BzoErr] DefinitionTable
checkProgram dt@(DefinitionTable defs files ids _) =
  let
      err0 :: [BzoErr]
      err0 = noOverloadTypes dt

      err1 :: [BzoErr]
      err1 = noUndefinedErrs dt

      --dfs  :: Either [BzoErr] (DefinitionTable, M.Map Text SymbolTable)
      --dfs  = modelProgram dt

      -- A NameTable will allow for limiting searches to specific namespaces.
      -- e.g, handling foo@Namespace requires this.
      ntab :: NameTable
      ntab = makeNameTable dt

      -- A Type Table will make it much easier to figure out the type of something.
      -- Good for faster type checking
      --ttab :: Map Int64 AbsType
      --ttab = ... make type table ...

      scopect :: [(Int64, Int, Int)]
      !scopect = L.scanl (\(_, _, n) (k, dx) -> (k, n, dx+n)) (0, 1, 1) $ L.map (\(k, d) -> (k, ctDefScopes d)) $ M.assocs defs

      scopetab :: ScopeTable
      !scopetab = initializeScopeTable $ (\(_,_,x) -> x) $ L.last scopect

      !x = trace (show scopetab) 0

      --dts' :: [(DefinitionTable, M.Map Text SymbolTable)]
      --dts' = rights [dfs]

      --err2 :: [BzoErr]
      --err2 = L.concat $ lefts [dfs]

      errs :: [BzoErr]
      errs = err0 ++ err1-- ++ err2
  in case (errs) of
      ([]) -> Right dt
      (er) -> Left  er
