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









-- FIXME
checkTypeHeader :: (TypeHeader, BzoSyntax) -> TypeHeader -> [BzoErr]
checkTypeHeader (th, (BzS_Cmpd p xs)) (TyHeader ys) =
  let
      isSameLength :: Bool
      isSameLength = (L.length xs) == (M.size ys)

      headerList :: [THeadAtom]
      headerList = L.map snd $ M.assocs ys

      allMatch :: [BzoErr]
      allMatch = []   -- FIXME: Add checking code here. Probably will need more function parameters for this.

  in case (isSameLength) of
      (False) -> [TypeErr p $ pack "Wrong number of parameters for type constructor"]
      (True)  -> []









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

initializeTypeHeader x = trace ("\n\nERROR:" ++ (show x) ++ "\n\n") TyHeader M.empty










makeType :: SymbolTable -> TypeHeader -> BzoSyntax -> Either [BzoErr] Type
makeType st th (BzS_Expr   p  [x]) = makeType st th x
makeType st th (BzS_Statement p x) = makeType st th x
makeType st th (BzS_Cmpd  p [x]) = makeType st th x
makeType st th (BzS_Poly  p [x]) = makeType st th x
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










checkType :: AttribTable -> SymbolTable -> (TypeHeader, Type) -> (TypeHeader, Type) -> [BzoErr]
checkType at st (th0, (CmpdType p xs)) (th1, (CmpdType _ ys)) =
  let
      xs' :: [(TypeHeader, Type)]
      xs' = L.zip (L.repeat th0) xs

      ys' :: [(TypeHeader, Type)]
      ys' = L.zip (L.repeat th1) ys

      xys :: [((TypeHeader, Type), (TypeHeader, Type))]
      xys = L.zip xs' ys'

      samelength :: [BzoErr]
      samelength = ife ((L.length xs) == (L.length ys)) [] [TypeErr p $ pack "Compound lengths do not match"]

  in samelength ++ (L.concatMap (\(x, y) -> checkType at st x y) xys)

-- TODO: this will have to be adapted to handle type classes, though that will
-- require a lot more effort, including a full typeclass checker implementation.
checkType at st (th0, (LtrlType p x)) (th1, (LtrlType _ y)) = ife (x == y) [] [TypeErr p $ pack "Types do not match"]

checkType at st (th0, (IntType  p x)) (th1, (IntType  _ y)) = ife (x == y) [] [TypeErr p $ pack "Ints do not match"]
checkType at st (th0, (FltType  p x)) (th1, (FltType  _ y)) = ife (x == y) [] [TypeErr p $ pack "Floats do not match"]
checkType at st (th0, (StrType  p x)) (th1, (StrType  _ y)) = ife (x == y) [] [TypeErr p $ pack "Strings do not match"]

checkType at st (th0, (IntType  p x)) (th1, (LtrlType  _ y)) = ife (checkAttrib at y checkIntAttrib x) [] [TypeErr p $ pack "Unexpected Integer"]
checkType at st (th0, (FltType  p x)) (th1, (LtrlType  _ y)) = ife (checkAttrib at y checkFltAttrib x) [] [TypeErr p $ pack "Unexpected Float"]
checkType at st (th0, (StrType  p x)) (th1, (LtrlType  _ y)) = ife (checkAttrib at y checkStrAttrib x) [] [TypeErr p $ pack "Unexpected String"]

checkType at st (th0, (FuncType _ w x)) (th1, (FuncType _ y z)) = (checkType at st (th0, w) (th1, y)) ++ (checkType at st (th0, x) (th1, z))

checkType at st (th0, (VoidType _  )) (th1, (VoidType _  )) = []

-- Sizeless arrays
checkType at st (th0, (ArryType _ _ x)) (th1, (ArryType _ 0 y)) = (checkType at st (th0, x) (th1, y))

-- Sized arrays
checkType at st (th0, (ArryType p m x)) (th1, (ArryType _ n y)) = (checkType at st (th0, x) (th1, y)) ++ (ife (m == n) [] [TypeErr p $ pack "Array lengths do not match"])

-- Nil values are just Empty arrays
checkType at st (th0, (VoidType _    )) (th1, (ArryType _ 0 _)) = []

-- Handle tuples cast to arrays
-- May need to add another case for checking "single element" arrays?
checkType at st (th0, (CmpdType p  xs)) (th1, (ArryType _ n y)) =
  (ife ((L.length xs) == (fromIntegral n)) [] [TypeErr p $ pack "Compound type has incorrect number of parameters"]) ++
  (L.concatMap (\x -> checkType at st (th0, x) (th1, y)) xs)

checkType at st (th0, (PolyType p []    )) (th1,      (PolyType _ ys)) = []

checkType at st (th0, (PolyType p (x:xs))) (th1, poly@(PolyType _ ys)) =
    (checkType at st (th0, x) (th1, poly)) ++ (checkType at st (th0, (PolyType p xs)) (th1, poly))

checkType at st t0 (th1, (PolyType _ xs)) = L.concatMap (\x -> checkType at st t0 (th1, x)) xs

checkType at st (th0, (MakeType _ [])) (th1, (MakeType _ [])) = []

checkType at st (th0, (MakeType p0 [ix,ex])) (th1, (MakeType p1 [iy,ey])) = [] -- NOTE: tempoary, please extend
{-  let


  in-}

checkType at st (th0, (MakeType p0 xs)) (th1, (MakeType p1 ys)) =
  let
      xlast = L.last xs
      ylast = L.last ys

      xinit = L.init xs
      yinit = L.init ys

      -- Also add check that parameters are valid

  in (ife ((L.length xs) == (L.length ys)) [] [TypeErr p0 $ pack "Incorrect nesting of make type"]) ++
     (checkType at st (th0, xlast) (th1, ylast)) ++
     (checkType at st (th0, (MakeType p0 xinit)) (th1, (MakeType p1 yinit)))

{-
TODO:
  > "Make Type" checking. Not 100% how to do this either yet.
  > TVar checking
-}










checkAttrib :: AttribTable -> Int64 -> (S.Set Atm_Attrib -> a -> Bool) -> a -> Bool
checkAttrib atab i fn x =
  case (M.lookup i atab) of
    Nothing -> False
    Just at -> fn at x

checkStrAttrib :: S.Set Atm_Attrib -> Text -> Bool
checkStrAttrib ats t = (S.member (AL_Str t) ats) || (S.member (AA_Str) ats)

checkIntAttrib :: S.Set Atm_Attrib -> Integer -> Bool
checkIntAttrib ats i
  | (i >=                 -128) && (i <=                  127) = (S.member (AL_Int i) ats) || (S.member (AA_I8 ) ats)
  | (i >=                    0) && (i <=                  255) = (S.member (AL_Int i) ats) || (S.member (AA_U8 ) ats)
  | (i >=               -32768) && (i <=                32767) = (S.member (AL_Int i) ats) || (S.member (AA_I16) ats)
  | (i >=                    0) && (i <=                65535) = (S.member (AL_Int i) ats) || (S.member (AA_U16) ats)
  | (i >=          -2147483648) && (i <=           2147483647) = (S.member (AL_Int i) ats) || (S.member (AA_I32) ats)
  | (i >=                    0) && (i <=           4294967295) = (S.member (AL_Int i) ats) || (S.member (AA_U32) ats)
  | (i >= -9223372036854775808) && (i <=  9223372036854775807) = (S.member (AL_Int i) ats) || (S.member (AA_I64) ats)
  | (i >=                    0) && (i <= 18446744073709551615) = (S.member (AL_Int i) ats) || (S.member (AA_U64) ats)
  | otherwise = False

-- Not really robust, but good enough for now I guess.
checkFltAttrib :: S.Set Atm_Attrib -> Double -> Bool
checkFltAttrib ats f = (S.member (AL_Real f) ats) || (not $ S.null $ S.intersection ats (S.fromList [AA_P8, AA_P16, AA_P32, AA_P64, AA_F16, AA_F32, AA_F64]))










type AttribTable = M.Map Int64 (S.Set Atm_Attrib)

getAttribs :: DefinitionTable -> AttribTable
getAttribs (DefinitionTable defs _ _ _) =
  let
      defs' :: [(Int64, Definition)]
      defs' = M.assocs defs

      tys   :: [(Int64, BzoSyntax)]
      tys   = L.map (\(i, d) -> (i, typesyntax d)) $ L.filter isTDef defs'

  in M.fromList $ L.map (\(i, s) -> (i, typeAttribs s)) tys
  where
        isTDef :: (a, Definition) -> Bool
        isTDef (_, (TypeSyntax _ _ _)) = True
        isTDef _ = False










typeAttribs :: BzoSyntax -> S.Set Atm_Attrib
typeAttribs (BzS_Statement _ x) = typeAttribs x
typeAttribs (BzS_Expr _ [x]) = typeAttribs x
typeAttribs (BzS_Poly _  xs) = L.foldl S.union (S.empty) $ L.map typeAttribs xs
typeAttribs (BzS_BTId _   i) =
  case (isBuiltinType i) of
    1 -> S.singleton AA_I8
    2 -> S.singleton AA_I16
    3 -> S.singleton AA_I32
    4 -> S.singleton AA_I64
    5 -> S.singleton AA_U8
    6 -> S.singleton AA_U16
    7 -> S.singleton AA_U32
    8 -> S.singleton AA_U64
    9 -> S.singleton AA_F16
    10-> S.singleton AA_F32
    11-> S.singleton AA_F64
    12-> S.singleton AA_I64
    13-> S.singleton AA_F64
    14-> S.singleton AA_Bl
    15-> S.singleton AA_Bl
    16-> S.singleton AA_Bl
    17-> S.singleton AA_Str
    18-> S.singleton AA_Str
    _ -> S.empty
typeAttribs (BzS_Int _ i) = S.singleton $ AL_Int  i
typeAttribs (BzS_Flt _ f) = S.singleton $ AL_Real f
typeAttribs (BzS_Str _ s) = S.singleton $ AL_Str  s
typeAttribs _                = S.empty

attribTrim :: S.Set Atm_Attrib -> S.Set Atm_Attrib
attribTrim s =
  let intset = S.fromList [AA_I8, AA_U8, AA_I16, AA_U16, AA_I32, AA_U32, AA_I64, AA_U64]
      fltset = S.fromList [AA_P8,        AA_P16, AA_F16, AA_P32, AA_F32, AA_P64, AA_F64]
      strset = S.fromList [AA_Str]

      isInt  = not $ S.null $ S.intersection s intset
      isFlt  = not $ S.null $ S.intersection s fltset
      isStr  = not $ S.null $ S.intersection s strset

  in S.fromList $ L.filter (\x ->
                              case x of
                                (AL_Int  _) -> not isInt
                                (AL_Real _) -> not isFlt
                                (AL_Str  _) -> not isStr
                                _  -> True) $ S.elems s










getDefType :: SymbolTable -> Definition -> Either [BzoErr] Type
getDefType st (FuncSyntax    _ _ f@(BzS_FnTypeDef  _ ps _ ty) _) = makeType st (initializeTypeHeader f) ty
getDefType st (TypeSyntax    _ _ t@(BzS_TypDef     _ ps _ ty)  ) = makeType st (initializeTypeHeader t) ty
getDefType st (TyClassSyntax _ _ c@(BzS_TyClassDef p ps _ fs)  ) =
  let tys = L.map (\f -> (fnid f, (initializeTypeHeader f), makeType st (initializeTypeHeader f) $ def f)) fs
      err = L.concat $ lefts $ L.map trd3 tys
      fts = L.map (\(a,b,t) -> (a,b, L.head $ rights [t])) $ L.filter (\(_,_,t) -> E.isRight t) tys
  in case err of
      [] -> Right $ TyCsType p fts
      er -> Left  er

getDefType st _                                                 = Right $ InvalidType










-- Nested Input Stuff
getNestedCmpd :: BzoSyntax -> [Int] -> Maybe BzoSyntax
getNestedCmpd expr                  [] = Just expr
getNestedCmpd (BzS_Expr      _ [x]) is = getNestedCmpd x is
getNestedCmpd (BzS_Statement _  x ) is = getNestedCmpd x is
getNestedCmpd (BzS_Cmpd _ xs) (i:is) =
  case (L.drop i xs) of
    []     -> Nothing
    (y:ys) -> getNestedCmpd y is
getNestedCmpd _               _      = Nothing










tagScopes :: (M.Map Text Int) -> ScopeTable -> [Definition] -> (ScopeTable, M.Map BzoPos Int)
tagScopes ftab st@(ScopeTable scs top) defs =
  let

      dfs :: ([Definition], [Definition], [Definition])
      dfs = L.foldl (\(a,b,c)(d,e,f)->(a++d, b++e, c++f)) ([], [], []) $
            L.map (\d -> case d of
                              f@(FuncSyntax  _ _ _ _) -> ([f], [], [])
                              t@(TypeSyntax    _ _ _) -> ([], [t], [])
                              c@(TyClassSyntax _ _ _) -> ([], [], [c])
                              _                       -> ([], [], [] )) defs

      flook :: (Text, a) -> (Int, a)
      flook (t, x) = (ftab M.! t, x)

      ---

      fns :: [(Int, (BzoPos, [BzoSyntax]))]
      fns = L.map flook $ L.map (\f -> (hostfile f, (pos $  ftyheader f, ((ftyheader f):(funcsyntax f))))) $ fst3 dfs

      tys :: [(Int, (BzoPos, [BzoSyntax]))]
      tys = L.map flook $ L.map (\t -> (hostfile t, (pos $ typesyntax t, [typesyntax t]))) $ snd3 dfs

      tcs :: [(Int, (BzoPos, [BzoSyntax]))]
      tcs = L.map flook $ L.map (\c -> (hostfile c, (pos $ typesyntax c, [typesyntax c]))) $ trd3 dfs

      ---

      scopes :: [(BzoPos, [BzoPos])]
      scopes = L.concatMap (\(i, (p, syn)) -> (L.concatMap (posScopes []) syn)) (fns ++ tys ++ tcs)

      posmap :: M.Map BzoPos Int
      posmap = M.fromList $ L.zip (L.map fst scopes) (L.map (+top) [1..])

      newtop :: Int
      newtop = (M.size posmap) + top

      scopes':: [(BzoPos, Int, Scope)]
      scopes'= L.map (\(p, ps) -> (p, posmap M.! p, Scope M.empty M.empty [(pack "#parent", (L.map (posmap M.!) ps))] )) scopes

      scopes'' ::[(Int, Scope)]
      scopes'' = L.map (\(p, i, s) ->
                          case s of
                            (Scope a b [(par, [])]) -> (i, (Scope a b [(par, [ftab M.! (fileName p)])]))
                            s'                      -> (i, s) ) scopes'

      scs'   :: M.Map Int Scope
      scs'   = M.union scs $ M.fromList scopes''

  in  (ScopeTable scs' newtop, posmap)










posScopes :: [BzoPos] -> BzoSyntax -> [(BzoPos, [BzoPos])]
posScopes ps (BzS_Block          p xs) = (p, ps):(L.concatMap (posScopes [p]) xs)
posScopes ps (BzS_Statement      _ x ) = posScopes ps x
posScopes ps (BzS_Lambda       _ _ x ) = posScopes ps x
posScopes ps (BzS_MapObj         _ x ) = posScopes ps x
posScopes ps (BzS_Expr           _ xs) = L.concatMap (posScopes ps) xs
posScopes ps (BzS_Cmpd           _ xs) = L.concatMap (posScopes ps) xs
posScopes ps (BzS_Poly           _ xs) = L.concatMap (posScopes ps) xs
posScopes ps (BzS_LispCall     _ f xs) = (posScopes ps f) ++ (L.concatMap (posScopes ps) xs)
posScopes ps (BzS_FnTypeDef  p _ _  _) = [(p, ps)]
posScopes ps (BzS_FunDef   p _ _ _  d) = (p, ps):(posScopes [p] d)
posScopes ps (BzS_TypDef     p _ _  d) = [(p, ps)]
posScopes ps (BzS_TyClassDef p _ _ ds) = (p, ps):(L.concatMap (\x -> [(pos x, [p])]) ds)
posScopes _ _ = []










mapDefTypes :: (M.Map Int64 Type) -> (M.Map Int64 Definition) -> (M.Map Int64 Definition)
mapDefTypes ttab dtab =
  let
      ttab' :: [(Int64, Type)]
      ttab' = M.assocs ttab

      dtab' :: [(Int64, Definition, Type)]
      dtab' = L.map (\(i, t) -> (i, dtab M.! i, t)) ttab'

      mkHeader :: BzoSyntax -> TypeHeader
      mkHeader (BzS_FnTypeDef _ ps _ _) = initializeTypeHeader ps
      mkHeader (BzS_TypDef    _ ps _ _) = initializeTypeHeader ps

  in M.fromList $ L.map (\(i,d,t) -> (i, case d of
                      FuncSyntax    i h fh fd -> FuncDef    i h (mkHeader      fh) t FuncPropEmpty []
                      TypeSyntax    i h    td -> TypeDef    i h (mkHeader      td)   TypePropEmpty t
                      TyClassSyntax i h    cd -> TyClassDef i h (TyHeader M.empty)   TClsPropEmpty [] )) dtab'










checkProgram :: DefinitionTable -> Either [BzoErr] DefinitionTable
checkProgram dt@(DefinitionTable defs files ids top) =
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

      ttab :: M.Map Int64 (Either [BzoErr] Type)
      ttab = M.map (\d -> getDefType (makeSymbolTable dt $ unpack $ hostfile d) d) defs

      ttab':: M.Map Int64 Type
      ttab'= M.map getRight ttab

      -- A Type Table will make it much easier to figure out the type of something.
      -- Good for faster type checking
      --ttab :: Map Int64 AbsType
      --ttab = ... make type table ...

      scopect :: [(Int64, Int, Int)]
      !scopect = L.scanl (\(_, _, n) (k, dx) -> (k, n, dx+n)) (0, 1, 1) $ L.map (\(k, d) -> (k, ctDefScopes d)) $ M.assocs defs

      scopetab :: ScopeTable
      filetab  :: M.Map Text Int
      (scopetab, filetab) = makeScopeTable dt
      -- !scopetab = initializeScopeTable $ (\(_,_,x) -> x) $ L.last scopect

      scopetab':: ScopeTable
      postab   :: M.Map BzoPos Int
      (!scopetab', !postab) = tagScopes filetab scopetab (M.elems defs)

      !x = debugmsg "Scopetab :" postab

      !y = debugmsglist "TTab: " $ M.assocs ttab

      --deftys :: [(Int64, Either [BzoErr] Type)]
      --deftys = L.map (\i df -> (i, getDefType scopetab' df)) $ M.assocs defs

      --dts' :: [(DefinitionTable, M.Map Text SymbolTable)]
      --dts' = rights [dfs]

      --err2 :: [BzoErr]
      --err2 = L.concat $ lefts [dfs]

      errs :: [BzoErr]
      errs = err0 ++ err1-- ++ err2
  in case (errs) of
      ([]) -> Right (DefinitionTable (mapDefTypes ttab' defs) files ids top)
      (er) -> Left  er