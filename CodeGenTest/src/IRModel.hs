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
  typeNodes   :: [([Int], TyId)],
  typehints   :: [Hint]  }
  deriving Show

data TypeRef = TypeRef [Int] TyId
  deriving Show

data TypeKind = PureTy | FXTy_SRSW | FXTy_PRSW | FXTy_SRPW | FXTy_PRPW
  deriving Show

type TyId = Int










data ConstantData
  = ConstString CnstId Text
  | ConstInt    CnstId Int
  | ConstBool   CnstId
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
    deriving Show










data Node
  = CastNode   { nout:: Int , ntyp:: TypeRef,   npar:: Int }
  | GetNode    { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int }
  | SetNode    { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int , npr2:: Int }
  | BinopNode  { nout:: Int , nbop:: BinopCode, npr0:: Int , npr1:: Int }
  | OpNode     { nout:: Int , nop :: OpCode,    npar:: Int }
  | ParNode    { nout:: Int , ntyp:: TypeRef }
  | RetNode    { nout:: Int , ntyp:: TypeRef ,  npar:: Int }
  | CallNode   { nots::[Int], ncal:: CallCode,  nfid:: FnId, nprs::[Int]}
  | HOFNode    { nout:: Int , nhof:: HOFCode ,  nrts:: [Int] }
  | PhiNode    { nots::[Int], npar:: Int     , nfn0:: FnId, nfn1:: FnId, nrts:: [Int] }
  | FuncNode   { nout:: Int , nfid:: FnId }
  | TypeNode   { nout:: Int , ntid:: TyId }
  | CondNode   { nout:: Int , ncnd:: CondCode,  npr0:: Int , npr1:: Int }
  deriving Show










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp| CttzOp | CtlzOp |
                  PCntOp |
                  CnstOp
                  deriving Show

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | ICmpOp |   -- Integer Ops
                  UAddOp | USubOp | UMulOp | UDivOp | UModOp | UCmpOp |   -- Unsigned Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FCmpOp |   -- Float Ops
                  NAddOp | NSubOp | NMulOp | NDivOp | NModOp | NCmpOp |   -- Unum Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  LShLOp | LShROp | AShROp |
                  FLogOp | FRootOp| FExpOp |
                  NLogOp | NRootOp| NExpOp | PowOp
                  deriving Show

data HOFCode    = MapHF  | FoldHF | RedcHF | ZipHF  | UZipHF | ScanHF |
                  ChnHF  | FiltHF
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
modelConst ((PI_Const    p x  ):irs) syms = (ConstBool   ((cnstSymbols syms) ! x)  ) : (modelConst irs syms)
modelConst ((PI_ConstStr p x s):irs) syms = (ConstString ((cnstSymbols syms) ! x) s) : (modelConst irs syms)
modelConst ((PI_ConstInt p x n):irs) syms = (ConstInt    ((cnstSymbols syms) ! x) n) : (modelConst irs syms)
modelConst (_                  :irs) syms = modelConst irs syms










modelFunc :: [IRParseItem] -> IRSymbols -> ([IRErr], [FuncData])
modelFunc [] syms = ([], [])
modelFunc ((PI_FnDef  p fnid pars def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters
      -- Model Contents
      (er0, nds) = modelNodes p syms def

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
      (er0, nds) = modelNodes p syms def

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
      (er0, nds) = modelNodes p syms def

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










modelNodes :: IRPos -> IRSymbols -> [IRParseItem] -> ([IRErr], [Node])
modelNodes ps syms irs =
  let (ers, nds) = L.foldl (modelNode syms) ([], []) irs

      -- TODO: Verify that all the nodes fit together nicely
      outs = L.concatMap getOuts nds
      outs'= L.nub outs
      er = if outs /= outs'
            then [(IRErr ps $ pack $ "Function contains repeated SSA variables: " ++ (show $ L.map (\x -> "#" ++ (show x)) (outs L.\\ outs')) ++ "\n")]
            else []
  in (er++ers, nds)
  where getOuts :: Node -> [Int]
        getOuts (CallNode os _ _ _  ) = os
        getOuts (PhiNode  os _ _ _ _) = os
        getOuts n                       = [nout n]









modelNode :: IRSymbols -> ([IRErr], [Node]) -> IRParseItem -> ([IRErr], [Node])
modelNode syms state (PI_Node p ns op pars) =
  case (ns, unpack op, L.reverse pars) of
    ([n], "input" , [t@(PI_Type tps tns tid)])                  -> appendEither (onRight       (ParNode  n)      (makeTypeRef syms t)) state
    ([n], "output", [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> RetNode  n x ix) (makeTypeRef syms t)) state
    ([n], "cast"  , [t@(PI_Type tps tns tid), (PI_Int ips ix)]) -> appendEither (onRight (\x -> CastNode n x ix) (makeTypeRef syms t)) state
    ([n], "iadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n IAddOp  ix jx)) state
    ([n], "isub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n ISubOp  ix jx)) state
    ([n], "imul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n IMulOp  ix jx)) state
    ([n], "idiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n IDivOp  ix jx)) state
    ([n], "imod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n IModOp  ix jx)) state
    ([n], "uadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UAddOp  ix jx)) state
    ([n], "usub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n USubOp  ix jx)) state
    ([n], "umul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UMulOp  ix jx)) state
    ([n], "udiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UDivOp  ix jx)) state
    ([n], "umod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n UModOp  ix jx)) state
    ([n], "fadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FAddOp  ix jx)) state
    ([n], "fsub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FSubOp  ix jx)) state
    ([n], "fmul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FMulOp  ix jx)) state
    ([n], "fdiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FDivOp  ix jx)) state
    ([n], "fmod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FModOp  ix jx)) state
    ([n], "nadd"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NAddOp  ix jx)) state
    ([n], "nsub"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NSubOp  ix jx)) state
    ([n], "nmul"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NMulOp  ix jx)) state
    ([n], "ndiv"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NDivOp  ix jx)) state
    ([n], "nmod"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NModOp  ix jx)) state
    ([n], "or"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n OrOp    ix jx)) state
    ([n], "xor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n AndOp   ix jx)) state
    ([n], "and"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n XorOp   ix jx)) state
    ([n], "lor"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n LOrOp   ix jx)) state
    ([n], "land"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n LAndOp  ix jx)) state
    ([n], "lxor"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n LXorOp  ix jx)) state
    ([n], "ls"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LSCond  ix jx)) state
    ([n], "gt"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GTCond  ix jx)) state
    ([n], "leq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n LECond  ix jx)) state
    ([n], "geq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n GECond  ix jx)) state
    ([n], "eq"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EQCond  ix jx)) state
    ([n], "neq"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NECond  ix jx)) state
    ([n], "ez"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n EZCond  ix jx)) state
    ([n], "nz"    , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (CondNode  n NZCond  ix jx)) state
    ([n], "lshl"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n LShLOp  ix jx)) state
    ([n], "lshr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n LShROp  ix jx)) state
    ([n], "ashr"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n AShROp  ix jx)) state
    ([n], "logf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FLogOp  ix jx)) state
    ([n], "rtf"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FRootOp ix jx)) state
    ([n], "expf"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n FExpOp  ix jx)) state
    ([n], "logn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NLogOp  ix jx)) state
    ([n], "rtn"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NRootOp ix jx)) state
    ([n], "expn"  , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n NExpOp  ix jx)) state
    ([n], "pow"   , [(PI_Int ips ix), (PI_Int jps jx)])         -> appendEither (Right (BinopNode n PowOp   ix jx)) state
    ([n], "abs"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AbsOp   ix   )) state
    ([n], "trnc"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n TrncOp  ix   )) state
    ([n], "wide"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n WideOp  ix   )) state
    ([n], "neg"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n NegOp   ix   )) state
    ([n], "not"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n NotOp   ix   )) state
    ([n], "lnot"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n LNotOp  ix   )) state
    ([n], "sqrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n SqrtOp  ix   )) state
    ([n], "cbrt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n CbrtOp  ix   )) state
    ([n], "lg2"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n Lg2Op   ix   )) state
    ([n], "lg10"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n Lg10Op  ix   )) state
    ([n], "sin"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n SinOp   ix   )) state
    ([n], "cos"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n CosOp   ix   )) state
    ([n], "tan"   , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n TanOp   ix   )) state
    ([n], "asin"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AsinOp  ix   )) state
    ([n], "acos"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AcosOp  ix   )) state
    ([n], "atan"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AtanOp  ix   )) state
    ([n], "sinh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n SinhOp  ix   )) state
    ([n], "cosh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n CoshOp  ix   )) state
    ([n], "tanh"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n TanhOp  ix   )) state
    ([n], "asinh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AsinhOp ix   )) state
    ([n], "acosh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AcoshOp ix   )) state
    ([n], "atanh" , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n AtanhOp ix   )) state
    ([n], "cttz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n CttzOp  ix   )) state
    ([n], "ctlz"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n CtlzOp  ix   )) state
    ([n], "pcnt"  , [(PI_Int ips ix)])                          -> appendEither (Right (OpNode    n PCntOp  ix   )) state

    (ns,  "phi" , f@(PI_Func p0 fn):g@(PI_Func p1 gn):(PI_Int ips ix):xs) ->
      let fnid = getFnid syms f
          gnid = getFnid syms g
          pars = makeIntList xs
          ers = (getLefts pars) ++ (lefts [fnid]) ++ (lefts [gnid])
          ret = (PhiNode ns ix (justRight fnid) (justRight gnid) (getRights pars))
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "call"  , f@(PI_Func ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns FnCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "prcall", f@(PI_Proc ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns PrCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (ns , "excall", f@(PI_Extn ps fn):xs)                       ->
      let fnid = getFnid syms f
          pars = makeIntList xs
          ers  = (getLefts pars) ++ (lefts [fnid])
          ret  = (CallNode ns ExCall (justRight fnid) $ getRights pars)
          (errs, nods) = state
      in case ers of
          [] -> (ers,   ret:nods)
          er -> (er ++ ers, nods)

    (_  , _       , _                                         ) -> appendEither (Left  (IRErr p $ pack "Unrecognized operation.\n")) state

modelNode syms state _ = ([], [])










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










initialSymbolTable :: IRSymbols
initialSymbolTable =
  let typeList :: [(Text, Int)]
      typeList = L.map (\(a, b) -> (pack a, fromIntegral b)) (
                 [("I8",   1), ("I16",  2), ("I32",  3), ("I64",  4),
                  ("U8",   5), ("U16",  6), ("U32",  7), ("U64",  8),
                  ("N16",  9), ("N32", 10), ("N64", 11),
                  ("F16", 12), ("F32", 13), ("F64", 14),
                  ("ASCII", 15), ("UTF8", 16), ("UTF16", 17), ("UTF32", 18),
                  ("Char",  19), ("UChar",20), ("Bl",    21)])
      typeList' = L.map swap typeList

  in IRSymbols M.empty M.empty (M.fromList typeList) (M.fromList typeList') M.empty M.empty 22













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
