module BzoChecker where
import BzoTypes
import HigherOrder
import Builtins
import Data.Text
import Data.Int
import Data.Either as E
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









-- Wow, this function is complex. I should dig through this later to see if it can be simplified.
-- Looks like generating a type visibility map sooner might strip a lot of this out.
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










-- Takes type parameters and returns an associated header
initializeTypeHeader :: BzoSyntax -> TypeHeader
initializeTypeHeader (BzS_Undefined p) = TyHeader M.empty
initializeTypeHeader (BzS_Expr _ [x])  = initializeTypeHeader x

initializeTypeHeader (BzS_TyVar p v)        =
  TyHeader $ M.fromList [(1, TVrAtom p v        []                                          )]

initializeTypeHeader (BzS_FilterObj p v fs) =
  TyHeader $ M.fromList [(1, TVrAtom p (sid v) (L.map (\t -> Constraint p $ UnresType t) fs))]

initializeTypeHeader (BzS_Cmpd _ vs) =
  let atoms = L.map makeAtom vs
  in  TyHeader $ M.fromList $ L.zip [1..] $ L.reverse atoms
  where
        makeAtom :: BzoSyntax -> THeadAtom
        makeAtom (BzS_TyVar     p v   ) =
          TVrAtom p v []

        makeAtom (BzS_FilterObj p v fs) =
          TVrAtom p (sid v) (L.map (\t -> Constraint p $ UnresType t) fs)

        makeAtom (BzS_Expr _ [x]) = makeAtom x

initializeTypeHeader (BzS_FnTypeDef _ ps fn (BzS_FnTy _ i o)) =
  let tyhead = initializeTypeHeader ps

      tvars :: [(Text, BzoPos)]
      tvars  = (getTVars i) ++ (getTVars o)

      vnames :: S.Set Text
      vnames = S.fromList $ L.map fst tvars

      tvatms :: [THeadAtom]
      tvatms = L.map (\(v,p) -> TVrAtom p v []) tvars
      tvatms'= L.nubBy (\(TVrAtom _ a _) (TVrAtom _ b _) -> a == b) $ L.filter (\(TVrAtom _ x _) -> S.member x vnames) tvatms

      key :: Int64
      key    = L.maximum $ [0] ++ (M.keys $ tvarmap tyhead)

      tvsold :: [(TVId, THeadAtom)]
      tvsold = M.assocs $ tvarmap tyhead

      tvsnew :: [(TVId, THeadAtom)]
      tvsnew = L.zip (L.map (key+) [1..]) tvatms'

      tvsall :: [(TVId, THeadAtom)] -- I don't fully trust the nub here, but it seems to mostly work.
      tvsall = L.nubBy (\(_, (TVrAtom _ a _)) (_, (TVrAtom _ b _)) -> a == b) $ tvsold ++ tvsnew

  in TyHeader $ M.fromList tvsall

initializeTypeHeader (BzS_TypDef _ ps ty tydef) =
  let tyhead = initializeTypeHeader ps

      tvars :: [(Text, BzoPos)]
      tvars = getTVars tydef

      vnames :: S.Set Text
      vnames = S.fromList $ L.map fst tvars

      tvatms :: [THeadAtom]
      tvatms = L.map (\(v,p) -> TVrAtom p v []) tvars
      tvatms'= L.nubBy (\(TVrAtom _ a _) (TVrAtom _ b _) -> a == b) $ L.filter (\(TVrAtom _ x _) -> S.member x vnames) tvatms

      key :: Int64
      key    = L.maximum $ [0] ++ (M.keys $ tvarmap tyhead)

      tvsold :: [(TVId, THeadAtom)]
      tvsold = M.assocs $ tvarmap tyhead

      tvsnew :: [(TVId, THeadAtom)]
      tvsnew = L.zip (L.map (key+) [1..]) tvatms'

      tvsall :: [(TVId, THeadAtom)] -- I don't fully trust the nub here, but it seems to mostly work.
      tvsall = L.nubBy (\(_, (TVrAtom _ a _)) (_, (TVrAtom _ b _)) -> a == b) $ tvsold ++ tvsnew

  in TyHeader $ M.fromList tvsall










makeType :: FileTable -> TypeHeader -> BzoSyntax -> Either [BzoErr] Type
makeType ft th (BzS_Expr   p  [x]) = makeType ft th x
makeType ft th (BzS_Statement p x) = makeType ft th x
makeType ft th (BzS_Cmpd  p [x]) = makeType ft th x
makeType ft th (BzS_Poly  p [x]) = makeType ft th x
makeType ft th (BzS_Cmpd  p  xs) = onAllPass (L.map (makeType ft th) xs) (\ys -> CmpdType p ys)
makeType ft th (BzS_Poly  p  xs) = onAllPass (L.map (makeType ft th) xs) (\ys -> PolyType p ys)
makeType ft th (BzS_Int   p   n) = Right (IntType  p n)
makeType ft th (BzS_Flt   p   n) = Right (FltType  p n)
makeType ft th (BzS_Str   p   s) = Right (StrType  p s)
makeType ft th (BzS_Nil   p )    = Right (VoidType p)
makeType ft th (BzS_BTId  p bit) = Right (BITyType p $ isBuiltinType bit)
makeType ft th (BzS_ArrayObj p s x) = onAllPass [makeType ft th x] (\[y] -> ArryType p s y)
makeType ft th (BzS_FnTy  p i o) =
  let i' = makeType ft th i
      o' = makeType ft th o
      io = rights [i', o']
      er = lefts  [i', o']
  in case (er, io) of
      ([], [it, ot]) -> Right $ FuncType p it ot
      (er,       _ ) -> Left  $ L.concat er

makeType ft th (BzS_Expr  p xs)  = onAllPass (L.map (makeType ft th) xs) (\ys -> MakeType p ys)
makeType ft th ty@(BzS_TyId  p   t) =
  let ids = resolveId ft t
  in case ids of
      []  -> Left [TypeErr p $ pack ("Type " ++ (unpack t) ++ " is undefined.")]
      [x] -> Right (LtrlType p x)
      xs  -> Left [TypeErr p $ pack ("Ambiguous reference to type " ++ (unpack t) ++ ": " ++ (show xs) ++ " / ")]   -- TODO: Redesign this error message
makeType ft (TyHeader tvs) (BzS_TyVar p   v) =
  let tvpairs = M.assocs tvs
      tvnames = L.map (\(n,atm) -> (n, Mb.fromMaybe (pack "") $ atomId atm)) tvpairs
      ids = L.map fst $ L.filter (\(n,atm) -> v == atm) tvnames
  in case ids of
      []  -> Left [TypeErr p $ pack ("Type Variable " ++ (unpack v) ++ " is not defined in the type header.")]
      [x] -> Right (TVarType p x)
      xs  -> Left [TypeErr p $ pack ("Ambiguous reference to type variable " ++ (unpack v) ++ ": " ++ (show xs) ++ " / ")]  -- TODO: Redesign this error message

makeType ft th (BzS_Id p f) =
  let fids = resolveId ft f
  in case fids of
      [] -> Left [TypeErr p $ pack $ "Function " ++ (unpack f) ++ " is not defined in this scope."]
      xs -> Right (FLitType p xs)

makeType ft th ty@(BzS_Undefined _) = Right (UnresType ty)
makeType ft th x = Left [TypeErr (pos x) $ pack $ "Malformed type expression: " ++ show x]










data SymbolTable = SymbolTable (M.Map Text FileTable) deriving Show

data FileTable   = FileTable   (M.Map Text [Int64])   deriving Show

resolveId :: FileTable -> Text -> [Int64]
resolveId (FileTable ds) d = Mb.fromMaybe [] $ M.lookup d ds










makeFileTable :: M.Map Text [Int64] -> BzoFileModel ([Int64], [Int64]) -> (Text, FileTable)
makeFileTable dmap (BzoFileModel _ fp _ (_, vis) _ _ _ _) =
  let
      visset:: S.Set Int64
      visset = S.fromList vis

      dlist :: M.Map Text [Int64]
      dlist = M.fromList $
              L.filter (\(n, is) -> (L.length is) > 0) $
              L.map (\(n, is) -> (n, L.filter (\v -> S.member v visset) is)) $
              M.assocs dmap

  in (pack fp, FileTable dlist)










makeSymbolTable :: DefinitionTable -> SymbolTable
makeSymbolTable (DefinitionTable defs files ids top) = SymbolTable $ M.fromList $ L.map (makeFileTable ids) files









makeTypes :: DefinitionTable -> Either [BzoErr] DefinitionTable
makeTypes dt@(DefinitionTable defs files ids top) =
  let
      syms :: SymbolTable
      syms = makeSymbolTable dt

      translateDef :: Definition -> Definition
      translateDef (FuncSyntax    fn host fty@(BzS_FnTypeDef  _ ps _ ft) fs) = (FuncDef    fn host (initializeTypeHeader fty) (UnresType $ ft) fs)
      translateDef (TypeSyntax    ty host tyd@(BzS_TypDef     _ ps _ td)   ) = (TypeDef    ty host (initializeTypeHeader tyd) (UnresType $ td)   )
      translateDef (TyClassSyntax tc host tcd@(BzS_TyClassDef _ ps _ td)   ) = (TyClassDef tc host (initializeTypeHeader  ps) $ L.map xformTCFunc td)

      xformTCFunc :: BzoSyntax -> (Text, TypeHeader, Type)
      xformTCFunc (BzS_FnTypeDef _ ps fn ft) = (fn, (initializeTypeHeader ps), (UnresType $ ft))

      -- TODO:
      -- -- Construct Types for all definitions
      -- -- Check that make types are all valid (e.g, nothing like "Int Bool" as a definition.)

      errs :: [BzoErr]
      errs = []

  in case (errs) of
      [] -> Right (DefinitionTable (M.map translateDef defs) files ids top)
      er -> Left  er










checkProgram :: DefinitionTable -> Either [BzoErr] DefinitionTable
checkProgram dt@(DefinitionTable defs files ids top) =
  let
      -- No type overloads
      err0 :: [BzoErr]
      err0 = noOverloadTypes dt

      -- No undefined types
      err1 :: [BzoErr]
      err1 = noUndefinedErrs dt

      --TODO:
        -- Construct namespace mapping
        -- Construct types and typeheaders for all functions, types, and type classes
        -- Check that types are valid - mostly that type constructors obey headers
        -- Construct type class models

      err2 :: [BzoErr]
      dt'  :: [DefinitionTable]
      (err2, dt') = splitEitherList $ makeTypes dt


      errs :: [BzoErr]
      errs = err0 ++ err1 ++ err2
  in case (errs) of
      ([]) -> Right $ L.head dt'
      (er) -> Left  er
