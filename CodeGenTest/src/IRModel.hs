module IRModel where
import Data.Text
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Either
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
  = CastNode     Int  TypeRef   Int
  | GetNode      Int  TypeRef   Int   Int
  | SetNode      Int  TypeRef   Int   Int   Int
  | BinopNode    Int  BinopCode Int   Int
  | OpNode       Int  OpCode    Int
  | ParNode      Int  TypeRef
  | RetNode      Int  TypeRef   Int
  | CallNode    [Int] CallCode  FnId [Int]
  | HOFNode      Int  HOFCode  [Int]
  | PhiNode      Int  CondCode  Int   Int   Int
  | FuncNode     Int  FnId
  | TypeNode     Int  TyId
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
      [] -> Right (syms, M.empty, M.empty, M.empty, [])
      er -> Left er










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
      (er0, nds) = modelNodes syms def

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

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, (FuncType ProcFn [] [] rfx wfx [] [] fnid'):fns)

modelFunc ((PI_ExDef  p fnid pars refx wefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters

      -- Model Effect Types
      ermsg = " is an undefined type, and cannot be used as an effect.\n"
      (er1, rfx) = undefTypes ermsg syms refx
      (er2, wfx) = undefTypes ermsg syms wefx

      -- Model Contents

      -- Model the rest of the list
      (ers, fns) = modelFunc irs syms
  in (ers ++ er1 ++ er2, (FuncType ExtrFn [] [] rfx wfx [] [] fnid'):fns)










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










modelNodes :: IRSymbols -> [IRParseItem] -> ([IRErr], [Node])
modelNodes syms irs =
  let (ers, nds) = L.foldl (modelNode syms) ([], []) irs

      -- TODO: Verify that all the nodes fit together nicely

  in (ers, nds)










modelNode :: IRSymbols -> ([IRErr], [Node]) -> IRParseItem -> ([IRErr], [Node])
modelNode syms state (PI_Node p ns op pars) =
  case (ns, unpack op, pars) of
    ([n], "input" , [t@(PI_Type tps tns tid)])                  -> appendEither (onRight       (ParNode n)      (makeTypeRef syms t)) state
    ([n], "output", [(PI_Int ips ix), t@(PI_Type tps tns tid)]) -> appendEither (onRight (\x -> RetNode n x ix) (makeTypeRef syms t)) state
    (_  , _       , _                                         ) -> appendEither (Left (IRErr p $ pack "Unrecognized operation.\n")) state










makeTypeRef :: IRSymbols -> IRParseItem -> Either IRErr TypeRef
makeTypeRef syms (PI_Type tps tns tid) =
  case (M.lookup tid $ typeSymbols syms) of
    Just tx -> Right $ TypeRef tns tx
    Nothing -> Left  $ IRErr   tps $ append tid $ pack " is an undefined type.\n"













getSymbols :: [IRParseItem] -> ([IRErr], IRSymbols)
getSymbols [] = ([], (IRSymbols M.empty M.empty M.empty M.empty M.empty M.empty 0))

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
