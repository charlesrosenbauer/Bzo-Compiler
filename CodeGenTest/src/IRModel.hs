module IRModel where
import Data.Text
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Tuple
import IRParser
import IRLexer
import Debug.Trace









-- Change later and just import HigherOrder.hs
ife :: Bool -> a -> a -> a
ife True  a b = a
ife False a b = b










appendPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
appendPair (x0, y0) (x1, y1) = (x0 ++ x1, y0 ++ y1)










appendEither :: Either a b -> ([a], [b]) -> ([a], [b])
appendEither (Left  x) (xs, ys) = (x:xs,   ys)
appendEither (Right y) (xs, ys) = (  xs, y:ys)










onLeft  :: (a -> c) -> Either a b -> Either c b
onLeft f (Left  l) = Left (f l)
onLeft f (Right r) = Right r










onRight :: (b -> c) -> Either a b -> Either a c
onRight f (Right r) = Right (f r)
onRight f (Left  l) = Left l










getRights :: Either a [b] -> [b]
getRights (Left  x) = []
getRights (Right r) = r










getLefts  :: Either [a] b -> [a]
getLefts  (Left  x) = x
getLefts  (Right r) = []










justRight :: Either a b -> b
justRight (Right x) = x










justLeft  :: Either a b -> a
justLeft  (Left  x) = x










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  readEffects :: [TyId],
  writeEffects:: [TyId],
  funchints   :: [Hint],
  funcNodes   :: [Node],
  funcId      :: FnId }
  deriving Show

data FunctionKind = PureFn | ProcFn | ExtrFn
  deriving Show

type FnId = Int










data TypeData = TypeData{
  typeKind    :: TypeKind,
  typeNodes   :: [TypeNode],
  typeSize    :: Int,
  typehints   :: [Hint]  }
  deriving Show

data TypeRef = TypeRef [Int] TyId | UndefType
  deriving Show

data TypeKind = PureTy | FXTy_SRSW | FXTy_PRSW | FXTy_SRPW | FXTy_PRPW
  deriving Show

type TyId = Int










data TypeNode
  = ElemNode      { tidx :: Int, tbyt :: Int,      ttyp :: TypeRef }
  | Contain1Node  { tidx :: Int, tcn1 :: Contain1, tty0 :: TypeRef }
  | Contain2Node  { tidx :: Int, tcn2 :: Contain2, tty0 :: TypeRef, tty1 :: TypeRef }
  | ImplNode      { tidx :: Int, timp :: ImplCode, tfnc :: FnId }
  deriving Show










data Contain1 = ListCont | SetCont  | BSetCont | RRBCont  |
                BoxCont  | QCont    | StkCont  | HeapCont
                deriving Show

data Contain2 = DictCont | HMapCont | AVLCont
                deriving Show

data ImplCode = ImplMap  | ImplSplit| ImplSFold| ImplPFold|
                ImplSScan| ImplPScan| ImplZip  | ImplUZip |
                ImplNext | ImplIndex| ImplHash | ImplEq   |
                ImplCmp  | ImplSerl | ImplDSerl| ImplSize
                deriving Show










data ConstantData
  = ConstStr  CnstId Text
  | ConstInt  CnstId Int
  | ConstBool CnstId      -- Probably going to remove this later
  deriving Show

type CnstId = Int










data Hint
  = HintInt Text Int
  | HintStr Text Text
  deriving Show










data IRSymbols
  = IRSymbols{
    funcSymbols  :: Map Text   FnId,
    funcSymbols' :: Map FnId   Text,
    typeSymbols  :: Map Text   TyId,
    typeSymbols' :: Map TyId   Text,
    cnstSymbols  :: Map Text   CnstId,
    cnstSymbols' :: Map CnstId Text,
    idTop        :: Int }










showIRSym :: IRSymbols -> String
showIRSym (IRSymbols fs fs' ts ts' cs cs' top) =
    "Func  Symbols: " ++ (L.concatMap (\x -> (show x) ++ "\n") $ M.assocs fs) ++ "\n\n" ++
    "Type  Symbols: " ++ (L.concatMap (\x -> (show x) ++ "\n") $ M.assocs ts) ++ "\n\n" ++
    "Const Symbols: " ++ (L.concatMap (\x -> (show x) ++ "\n") $ M.assocs cs) ++ "\n\n" ++
    "Id# top: " ++ (show top) ++ "\n\n\n"
instance Show IRSymbols where show = showIRSym










data Node
  = CastNode   { nout:: Int , ntyp:: TypeRef,   npar:: Int }
  | ConstNode  { nout:: Int , ntyp:: TypeRef,   csid:: CnstId }
  | GetNode    { nout:: Int , ntyp:: TypeRef,   npar:: Int }
  | SetNode    { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int }
  | BinopNode  { nout:: Int , ntyp:: TypeRef,   nbop:: BinopCode, npr0:: Int , npr1:: Int }
  | TrinopNode { nout:: Int , ntyp:: TypeRef,   ntop:: TrinopCode,npr0:: Int , npr1:: Int , npr2:: Int }
  | OpNode     { nout:: Int , ntyp:: TypeRef,   nop :: OpCode,    npar:: Int }
  | ParNode    { nout:: Int , ntyp:: TypeRef }
  | RetNode    { nout:: Int , ntyp:: TypeRef ,  npar:: Int }
  | CallNode   { nots::[Int], ntps::[TypeRef],  ncal:: CallCode,  nfid:: FnId, nprs::[Int]}
  | HOFNode    { nots::[Int], ntps::[TypeRef],  nhof:: HOFCode ,  nrts:: [Int],nfns::[FnId]}
  | PhiNode    { nots::[Int], ntps::[TypeRef],  npar:: Int     , nfn0:: FnId, nfn1:: FnId, nrts:: [Int] }
  | CondNode   { nout:: Int , ncnd:: CondCode,  npr0:: Int , npr1:: Int }
  deriving Show










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp| CttzOp | CtlzOp |
                  CeilOp | FloorOp| RoundOp|
                  PCntOp | BRevsOp| BSwapOp|
                  CnstOp
                  deriving Show

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | ICmpOp | IMinOp | IMaxOp |  -- Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FCmpOp | FMinOp | FMaxOp |  -- Float Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  LShLOp | LShROp | AShROp |
                  FLogOp | FRootOp| FExpOp |
                  NLogOp | NRootOp| NExpOp | PowOp
                  deriving Show

data TrinopCode = FFMAOp | FFMSOp | IFMAOp | IFMSOp
                  deriving Show

-- Some of these *technically* aren't higher-order functions, but I'm including them anyway.
data HOFCode    = MapHF  | SFoldHF| PFoldHF| SScanHF| PScanHF| ZipHF  | UnzipHF| FilterHF|
                  RevsHF | SortHF | TakeHF | ReptHF | ConctHF| AppndHF| ArrayHF| SliceHF |
                  AdjstHF| SplitHF| RmovHF | DropHF | TailHF | AnyHF  | AllHF  | NoneHF  |
                  InsrtHF| IndexHF| IterHF
                  deriving Show

data CondCode   = LSCond | GTCond | EQCond | NECond | LECond | GECond |
                  NZCond | EZCond
                  deriving Show

data CallCode   = FnCall | ExCall | PrCall
                  deriving Show










modelIR :: IRParseItem -> Either [IRErr] (IRSymbols,
                                          Map FnId FuncData,
                                          Map TyId TypeData,
                                          Map CnstId ConstantData,
                                          [Hint])
modelIR irs =
  let (errs, syms) = getSymbols   [irs]
      fns          = getFunctions [irs]
      tys          = getTypes     [irs]
      cts          = getConstants [irs]
      cts'         = modelConst    cts syms
      (fers, fns') = modelFunc     fns syms
  in case (fers ++ errs) of
      [] -> Right (syms, (makeFuncMap fns'), M.empty, M.empty, [])
      er -> Left er










makeFuncMap :: [FuncData] -> Map FnId FuncData
makeFuncMap fs = M.fromList $ L.map (\f@(FuncType _ _ _ _ _ _ _ fnid) -> (fnid, f)) fs









getFunctions :: [IRParseItem] -> [IRParseItem]
getFunctions [] = []
getFunctions ((PI_Defs _ defs)          :irs) = getFunctions (irs ++ defs)
getFunctions (fn@(PI_FnDef _ _ _ _    ) :irs) = fn : (getFunctions irs)
getFunctions (fn@(PI_PrDef _ _ _ _ _ _) :irs) = fn : (getFunctions irs)
getFunctions (fn@(PI_ExDef _ _ _ _ _ _) :irs) = fn : (getFunctions irs)
getFunctions (x                         :irs) =      (getFunctions irs)










getTypes :: [IRParseItem] -> [IRParseItem]
getTypes [] = []
getTypes ((PI_Defs _ defs)      :irs) = getTypes (irs ++ defs)
getTypes (fn@(PI_TyDef _ _ _ _) :irs) = fn : (getTypes irs)
getTypes (x                     :irs) =      (getTypes irs)










getConstants :: [IRParseItem] -> [IRParseItem]
getConstants [] = []
getConstants ((PI_Defs _ defs)       :irs) = getTypes (irs ++ defs)
getConstants (fn@(PI_Const    _ _  ) :irs) = fn : (getTypes irs)
getConstants (fn@(PI_ConstInt _ _ _) :irs) = fn : (getTypes irs)
getConstants (fn@(PI_ConstStr _ _ _) :irs) = fn : (getTypes irs)
getConstants (x                      :irs) =      (getTypes irs)










modelConst :: [IRParseItem] -> IRSymbols -> [ConstantData]
modelConst [] syms = []
modelConst ((PI_Const    p x  ):irs) syms = (ConstBool ((cnstSymbols syms) ! x)  ) : (modelConst irs syms)
modelConst ((PI_ConstStr p x s):irs) syms = (ConstStr  ((cnstSymbols syms) ! x) s) : (modelConst irs syms)
modelConst ((PI_ConstInt p x n):irs) syms = (ConstInt  ((cnstSymbols syms) ! x) n) : (modelConst irs syms)
modelConst (_                  :irs) syms = modelConst irs syms










modelFunc :: [IRParseItem] -> IRSymbols -> ([IRErr], [FuncData])
modelFunc [] syms = ([], [])
modelFunc ((PI_FnDef  p fnid pars def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters
      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (er0 ++ ers, ((FuncType PureFn [] [] [] [] [] nds fnid'):fns))

modelFunc ((PI_PrDef  p fnid pars refx wefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters

      -- Model Effect Types
      ermsg = " is an undefined type, and cannot be used as an effect.\n"
      (er1, rfx) = undefTypes ermsg syms refx
      (er2, wfx) = undefTypes ermsg syms wefx

      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, (FuncType ProcFn [] [] rfx wfx [] nds fnid'):fns)

modelFunc ((PI_ExDef  p fnid pars refx wefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters

      -- Model Effect Types
      ermsg = " is an undefined type, and cannot be used as an effect.\n"
      (er1, rfx) = undefTypes ermsg syms refx
      (er2, wfx) = undefTypes ermsg syms wefx

      -- Model Contents
      (er0, nds) = modelFuncNodes p syms def

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, (FuncType ExtrFn [] [] rfx wfx [] nds fnid'):fns)










-- | Assumes [IRParseItem] is [PI_Type]
undefTypes :: String -> IRSymbols -> [IRParseItem] -> ([IRErr], [TyId])
undefTypes ermsg syms []      = ([], [])
undefTypes ermsg syms (x@(PI_Type p _ t):irs) =
  let (ers, ids) = undefTypes ermsg syms irs
      tyid       = M.lookup t $ typeSymbols syms
  in case tyid of
      Nothing -> ((IRErr p $ pack $ (unpack $ t) ++ ermsg):ers, ids)
      Just ty -> (ers, ty:ids)

undefTypes ermsg syms (_:irs) = undefTypes ermsg syms irs










modelFuncNodes :: IRPos -> IRSymbols -> [IRParseItem] -> ([IRErr], [Node])
modelFuncNodes ps syms irs =
  let (ers, nds) = L.foldl (modelFuncNode syms) ([], []) irs

      -- TODO: Verify that all the nodes fit together nicely
      (outss, inss) = L.unzip $ L.map (\nd -> (getOuts nd, getIns nd)) nds
      outs = L.concat outss
      outs'= L.nub outs
      er0  = if outs /= outs'
              then [(IRErr ps $ pack $ "Function contains repeated SSA variables: " ++ (show $ L.map (\x -> "#" ++ (show x)) (outs L.\\ outs')) ++ "\n")]
              else []

      iopairs = L.zip inss outss
      er1  = catMaybes $ L.map (\(i, o) -> checkIns ps (o, i, outs)) iopairs
  in (er0++er1++ers, nds)
  where getOuts :: Node -> [Int]
        getOuts (CallNode os _ _ _ _  ) = os
        getOuts (PhiNode  os _ _ _ _ _) = os
        getOuts (HOFNode  os _ _ _ _)   = os
        getOuts n                       = [nout n]

        getIns  :: Node -> [Int]
        getIns  (SetNode   _ _     p0 p1   ) = [p0, p1]
        getIns  (BinopNode _ _ _   p0 p1   ) = [p0, p1]
        getIns  (TrinopNode  _ _ _ p0 p1 p2) = [p0, p1, p2]
        getIns  (ParNode     _ _)            = []
        getIns  (ConstNode _ _ _)            = []
        getIns  (CallNode  _ _ _ _ ps)       = ps
        getIns  (HOFNode   _ _ _ ps _)       = ps
        getIns  (PhiNode   _ _ p0 _ _ ps)    = (p0:ps)
        getIns  (CondNode  _ _ p0 p1)        = [p0, p1]
        getIns  node                         = [npar node]

        checkIns :: IRPos -> ([Int], [Int], [Int]) -> Maybe IRErr
        checkIns ps ([],  _,  _) = Nothing
        checkIns ps (os, is, xs) =
          let osmin   = L.minimum os
              isfail0 = L.filter (\x -> x >= osmin) is
              isfail1 = L.filter (\x -> L.notElem x xs) is
          in case (isfail0, isfail1) of
              ([], []) -> Nothing
              -- These error messages suck, but IR code isn't meant to be a serious language, so who cares?
              (f0, []) -> Just $ IRErr ps $ pack ("Cyclical dependencies in function definition, involving variables " ++ (show f0) ++ "\n")
              ([], f1) -> Just $ IRErr ps $ pack ("Calls to undefined variables in function definition: " ++ (show f1) ++ "\n")
              (f0, f1) -> Just $ IRErr ps $ pack ("Cyclical dependencies, and calls to undefined variables in function definition: " ++ (show f1) ++ "\n")










modelFuncNode :: IRSymbols -> ([IRErr], [Node]) -> IRParseItem -> ([IRErr], [Node])
modelFuncNode syms state (PI_Node p ns op pars) =
  case (ns, unpack op, L.reverse pars) of
    ([n], "input" , [t@(PI_Type tps tns tid)])                  -> appendEither (onRight       (ParNode  n)      (makeTypeRef syms t)) state
    ([n], "output", [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> RetNode  n x ix) (makeTypeRef syms t)) state
    ([n], "cast"  , [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> CastNode n x ix) (makeTypeRef syms t)) state

    ([n], "load"  , [t@(PI_Type tps tns tid), (PI_Int ips ix)                 ]) -> appendEither (onRight (\x -> GetNode n x ix   ) (makeTypeRef syms t)) state
    ([n], "stor"  , [t@(PI_Type tps tns tid), (PI_Int ips ix), (PI_Int jps jx)]) -> appendEither (onRight (\x -> SetNode n x ix jx) (makeTypeRef syms t)) state

    ([n], "ifma"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType IFMAOp  ix jx kx)) state
    ([n], "ifms"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType IFMSOp  ix jx kx)) state
    ([n], "ffma"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType FFMAOp  ix jx kx)) state
    ([n], "ffms"  , [(PI_Int ips ix), (PI_Int jps jx), (PI_Int kps kx)]) -> appendEither (Right (TrinopNode n UndefType FFMSOp  ix jx kx)) state

    ([n], "iadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IAddOp  ix jx)) state
    ([n], "isub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType ISubOp  ix jx)) state
    ([n], "imul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMulOp  ix jx)) state
    ([n], "idiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IDivOp  ix jx)) state
    ([n], "imod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IModOp  ix jx)) state
    ([n], "imax"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMaxOp  ix jx)) state
    ([n], "imin"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType IMinOp  ix jx)) state
    ([n], "fadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FAddOp  ix jx)) state
    ([n], "fsub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FSubOp  ix jx)) state
    ([n], "fmul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMulOp  ix jx)) state
    ([n], "fdiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FDivOp  ix jx)) state
    ([n], "fmod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FModOp  ix jx)) state
    ([n], "fmax"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMaxOp  ix jx)) state
    ([n], "fmin"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FMinOp  ix jx)) state

    ([n], "or"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType OrOp    ix jx)) state
    ([n], "xor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType AndOp   ix jx)) state
    ([n], "and"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType XorOp   ix jx)) state
    ([n], "lor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LOrOp   ix jx)) state
    ([n], "land"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LAndOp  ix jx)) state
    ([n], "lxor"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LXorOp  ix jx)) state
    ([n], "ls"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LSCond  ix jx)) state
    ([n], "gt"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GTCond  ix jx)) state
    ([n], "leq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LECond  ix jx)) state
    ([n], "geq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GECond  ix jx)) state
    ([n], "eq"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EQCond  ix jx)) state
    ([n], "neq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NECond  ix jx)) state
    ([n], "ez"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EZCond  ix jx)) state
    ([n], "nz"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NZCond  ix jx)) state
    ([n], "lshl"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LShLOp  ix jx)) state
    ([n], "lshr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType LShROp  ix jx)) state
    ([n], "ashr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType AShROp  ix jx)) state
    ([n], "cttz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CttzOp  ix   )) state
    ([n], "ctlz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CtlzOp  ix   )) state
    ([n], "pcnt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType PCntOp  ix   )) state
    ([n], "brev"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType BRevsOp ix   )) state
    ([n], "bswap" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType BSwapOp ix   )) state

    ([n], "logf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FLogOp  ix jx)) state
    ([n], "rtf"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FRootOp ix jx)) state
    ([n], "expf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType FExpOp  ix jx)) state
    ([n], "logn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NLogOp  ix jx)) state
    ([n], "rtn"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NRootOp ix jx)) state
    ([n], "expn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType NExpOp  ix jx)) state
    ([n], "pow"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UndefType PowOp   ix jx)) state
    ([n], "abs"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AbsOp   ix   )) state

    ([n], "trnc"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TrncOp  ix   )) state
    ([n], "wide"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType WideOp  ix   )) state
    ([n], "neg"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType NegOp   ix   )) state
    ([n], "not"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType NotOp   ix   )) state
    ([n], "lnot"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType LNotOp  ix   )) state
    ([n], "sqrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SqrtOp  ix   )) state
    ([n], "cbrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CbrtOp  ix   )) state
    ([n], "lg2"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType Lg2Op   ix   )) state
    ([n], "lg10"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType Lg10Op  ix   )) state
    ([n], "sin"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SinOp   ix   )) state
    ([n], "cos"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CosOp   ix   )) state
    ([n], "tan"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TanOp   ix   )) state
    ([n], "asin"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AsinOp  ix   )) state
    ([n], "acos"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AcosOp  ix   )) state
    ([n], "atan"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AtanOp  ix   )) state
    ([n], "sinh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType SinhOp  ix   )) state
    ([n], "cosh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CoshOp  ix   )) state
    ([n], "tanh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType TanhOp  ix   )) state
    ([n], "asinh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AsinhOp ix   )) state
    ([n], "acosh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AcoshOp ix   )) state
    ([n], "atanh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType AtanhOp ix   )) state
    ([n], "ceil"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType CeilOp  ix   )) state
    ([n], "floor" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType FloorOp ix   )) state
    ([n], "round" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n UndefType RoundOp ix   )) state

    ([n], "const" , [c@(PI_Const _ cid)])                       -> appendEither (onRight (\x -> ConstNode n UndefType x) (getConstId syms c)) state

    ([n]  , "zip"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ZipHF   [ix, jx] [])) state
    ([m,n], "unzip" , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [m,n] [] UnzipHF [ix    ] [])) state
    ([n]  , "revs"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] RevsHF  [ix    ] [])) state
    ([n]  , "sort"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] SortHF  [ix    ] [])) state
    ([n]  , "tail"  , [(PI_Int ips ix)])                          -> appendEither (Right (HOFNode    [  n] [] TailHF  [ix    ] [])) state
    ([n]  , "take"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] TakeHF  [ix, jx] [])) state
    ([n]  , "drop"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] DropHF  [ix, jx] [])) state
    ([n]  , "rept"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ReptHF  [ix, jx] [])) state
    ([n]  , "concat", [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] ConctHF [ix, jx] [])) state
    ([n]  , "append", [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (HOFNode    [  n] [] AppndHF [ix, jx] [])) state

    ([n], "map"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] MapHF [ix])
    ([n], "sfold" , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] SFoldHF [ix, jx])
    ([n], "pfold" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] PFoldHF [ix])
    ([n], "sscan" , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] SScanHF [ix, jx])
    ([n], "pscan" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] PScanHF [ix])
    ([n], "filter" , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                -> hofmodel syms state [f] (HOFNode [n] [] FilterHF [ix])
    ([n], "any"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] AnyHF [ix])
    ([n], "all"   , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] AllHF [ix])
    ([n], "none"  , [f@(PI_Func p0 fn), (PI_Int p1 ix)])                 -> hofmodel syms state [f] (HOFNode [n] [] NoneHF [ix])
    ([n], "iter"  , [f@(PI_Func p0 fn), (PI_Int p1 ix), (PI_Int p2 jx)]) -> hofmodel syms state [f] (HOFNode [n] [] IterHF [ix, jx])

    (ns,  "phi"   , f@(PI_Func p0 fn):g@(PI_Func p1 gn):(PI_Int ips ix):xs) ->
      let fnid = getFnid syms f
          gnid = getFnid syms g
          pars = makeIntList xs
          ers = (getLefts pars) ++ (lefts [fnid]) ++ (lefts [gnid])
          ret = (PhiNode ns [] ix (justRight fnid) (justRight gnid) (getRights pars))
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)


    (ns , "call"  , f@(PI_Func ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] FnCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "prcall", f@(PI_Proc ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] PrCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "excall", f@(PI_Extn ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns [] ExCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (_  , _       , _                                         ) -> appendEither (Left  (IRErr p $ pack "Unrecognized operation.\n")) state

  where hofmodel :: IRSymbols -> ([IRErr], [Node]) -> [IRParseItem] -> ([FnId] -> Node) -> ([IRErr], [Node])
        hofmodel syms (errs, nods) fs makeNode =
          let fnids = L.map (getFnid syms) fs
              ers = lefts fnids
              ret = makeNode (rights fnids)
          in case ers of
              [] -> (ers,   ret:nods)
              er -> (er ++ ers, nods)


modelFuncNode syms state _ = state










makeIntList :: [IRParseItem] -> Either [IRErr] [Int]
makeIntList []                  = Right []
makeIntList ((PI_Int ips ix):irs) =
  case (makeIntList irs) of
    Left ers -> Left ers
    Right xs -> Right (ix:xs)

makeIntList (ir:irs) =
  case (makeIntList irs) of
    Left ers -> Left ((IRErr (ppos ir) $ pack "Invalid operand.\n"):ers)
    Right xs -> Left [IRErr (ppos ir) $ pack "Invalid operand.\n"]










getFnid :: IRSymbols -> IRParseItem -> Either IRErr FnId
getFnid syms (PI_Func fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined function.\n"

getFnid syms (PI_Proc fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined procedure.\n"

getFnid syms (PI_Extn fps fid) =
  case (M.lookup fid $ funcSymbols syms) of
    Just fx -> Right fx
    Nothing -> Left  $ IRErr fps $ append fid $ pack " is an undefined external function.\n"







makeTypeRef :: IRSymbols -> IRParseItem -> Either IRErr TypeRef
makeTypeRef syms (PI_Type tps tns tid) =
  case (M.lookup tid $ typeSymbols syms) of
    Just tx -> Right $ TypeRef tns tx
    Nothing -> Left  $ IRErr   tps $ append tid $ pack " is an undefined type.\n"










getConstId :: IRSymbols -> IRParseItem -> Either IRErr CnstId
getConstId syms (PI_Const cps cid) =
  case (M.lookup cid $ cnstSymbols syms) of
    Just cx -> Right $ cx
    Nothing -> Left  $ IRErr   cps $ append cid $ pack " is an undefined constant.\n"










modelTypeNode :: IRSymbols -> ([IRErr], [TypeNode]) -> IRParseItem -> ([IRErr], [TypeNode])
modelTypeNode syms state (PI_Node p ns op pars) =
  case (ns, unpack op, L.reverse pars) of
    ([n], "element" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (ElemNode     n 0        ) (makeTypeRef syms t)) state
    ([n], "contlist", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n ListCont ) (makeTypeRef syms t)) state
    ([n], "contset" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n SetCont  ) (makeTypeRef syms t)) state
    ([n], "contbset", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n BSetCont ) (makeTypeRef syms t)) state
    ([n], "contrrb" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n RRBCont  ) (makeTypeRef syms t)) state
    ([n], "contbox" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n BoxCont  ) (makeTypeRef syms t)) state
    ([n], "contq"   , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n QCont    ) (makeTypeRef syms t)) state
    ([n], "contstk" , [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n StkCont  ) (makeTypeRef syms t)) state
    ([n], "contheap", [t@(PI_Type _ _ _)])                        -> appendEither (onRight       (Contain1Node n HeapCont ) (makeTypeRef syms t)) state

    -- TODO: Get multiple types working
    --([n], "contdict", [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> appendEither (onRight       (Contain2Node n HeapCont t0 t1) (makeTypeRef syms t)) state
    --([n], "conthmap", [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> appendEither (onRight       (Contain2Node n HeapCont t0 t1) (makeTypeRef syms t)) state
    --([n], "contavl" , [t0@(PI_Type _ _ _), t1@(PI_Type _ _ _)])   -> appendEither (onRight       (Contain2Node n HeapCont t0 t1) (makeTypeRef syms t)) state

    ([n], "implmap"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplMap)
    ([n], "implsplit" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSplit)
    ([n], "implsfold" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSFold)
    ([n], "implpfold" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplPFold)
    ([n], "implsscan" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSScan)
    ([n], "implpscan" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplPScan)
    ([n], "implzip"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplZip)
    ([n], "impluzip"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplUZip)
    ([n], "implnext"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplNext)
    ([n], "implindex" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplIndex)
    ([n], "implhash"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplHash)
    ([n], "impleq"    , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplEq)
    ([n], "implcmp"   , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplCmp)
    ([n], "implserl"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSerl)    -- Serialize
    ([n], "impldserl" , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplDSerl)   -- Deserialize
    ([n], "implsize"  , [f@(PI_Func _ _)]) -> modelImpl syms state f (ImplNode n ImplSize)

  where modelImpl :: IRSymbols -> ([IRErr], [TypeNode]) -> IRParseItem -> (FnId -> TypeNode) -> ([IRErr], [TypeNode])
        modelImpl syms (errs, nods) f@(PI_Func p0 fn) makeNode =
          let fnid = getFnid syms f
              ers  = lefts [fnid]
              ret  = makeNode (justRight fnid)
          in case ers of
              [] -> (ers,   ret:nods)
              er -> (er ++ ers, nods)

modelTypeNode syms state _ = state








initialSymbolTable :: IRSymbols
initialSymbolTable =
  let typeList :: [(Text, Int)]
      typeList = L.map (\(a, b) -> (pack a, fromIntegral b)) (
                 [("I8",   1), ("I16",  2), ("I32",  3), ("I64",  4),
                  ("U8",   5), ("U16",  6), ("U32",  7), ("U64",  8),
                  ("N16",  9), ("N32", 10), ("N64", 11),
                  ("F16", 12), ("F32", 13), ("F64", 14),
                  ("ASCII", 15), ("UTF8", 16), ("UTF16", 17), ("UTF32", 18),
                  ("Char",  19), ("UChar",20), ("Bl",    21),
                  ("I8v2",  22), ("I16v2", 23), ("I32v2", 24), ("I64v2", 25),
                  ("U8v2",  26), ("U16v2", 27), ("U32v2", 28), ("U64v2", 29),
                  ("N16v2", 30), ("N32v2", 31), ("N64v2", 32),
                  ("F16v2", 33), ("F32v2", 34), ("F64v2", 35),
                  ("I8v4",  36), ("I16v4", 37), ("I32v4", 38), ("I64v4", 39),
                  ("U8v4",  40), ("U16v4", 41), ("U32v4", 42), ("U64v4", 43),
                  ("N16v4", 44), ("N32v4", 45), ("N64v4", 46),
                  ("F16v4", 47), ("F32v4", 48), ("F64v4", 49),
                  ("I8v8",  50), ("I16v8", 51), ("I32v8", 52), ("I64v8", 53),
                  ("U8v8",  54), ("U16v8", 55), ("U32v8", 56), ("U64v8", 57),
                  ("N16v8", 58), ("N32v8", 59), ("N64v8", 60),
                  ("F16v8", 61), ("F32v8", 62), ("F64v8", 63),
                  ("I8v16", 64), ("I16v16",65), ("I32v16",66), ("I64v16",67),
                  ("U8v16", 68), ("U16v16",69), ("U32v16",70), ("U64v16",71),
                  ("N16v16",72), ("N32v16",73), ("N64v16",74),
                  ("F16v16",75), ("F32v16",76), ("F64v16",77)])
      typeList' = L.map swap typeList

  in IRSymbols M.empty M.empty (M.fromList typeList) (M.fromList typeList') M.empty M.empty 78













getSymbols :: [IRParseItem] -> ([IRErr], IRSymbols)
getSymbols [] = ([], initialSymbolTable)

getSymbols ((PI_Defs _ defs):irs) = getSymbols (irs ++ defs)

getSymbols ((PI_FnDef p fnid _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' top'))

getSymbols ((PI_TyDef p tyid _ _):irs) =
  let (errs, (IRSymbols fns fns'  tys  tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup tyid tys) top (top+1)
      ntys' = M.insert top' tyid tys'
      ntys  = M.insert tyid top' tys
      errs' = ife (top /= top') errs ((IRErr p $ append tyid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' ntys ntys' cns cns' (top+1)))

getSymbols ((PI_PrDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' (top+1)))

getSymbols ((PI_ExDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' cns cns' (top+1)))

getSymbols ((PI_Const p cid):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols ((PI_ConstInt p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols ((PI_ConstStr p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ncns ncns' (top+1)))

getSymbols (_:irs) = getSymbols irs
