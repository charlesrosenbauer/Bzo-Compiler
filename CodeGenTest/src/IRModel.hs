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










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  funchints   :: [Hint],
  funcNodes   :: [Node],
  funcId      :: FnId }
  deriving Show

data FunctionKind = PureFn | ProcFn | ExtrFn
  deriving Show

type FnId = Int










data TypeData = TypeData{
  typeNodes   :: [([Int], TyId)],
  typehints   :: [Hint]  }
  deriving Show

data TypeRef = TypeRef [Int] TyId
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
      cts'         = modelConst    cts
  in case errs of
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
      (ers, fns) = modelFunc irs syms
  in (ers, (FuncType PureFn [] [] [] [] fnid'):fns)

modelFunc ((PI_PrDef  p fnid pars iefx oefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters
      -- Model Effects
      -- Model Contents
      (ers, fns) = modelFunc irs syms
  in (ers, (FuncType ProcFn [] [] [] [] fnid'):fns)

modelFunc ((PI_ExDef  p fnid pars iefx oefx def):irs) syms =
  let fnid' = (funcSymbols syms) ! fnid
      -- Model Parameters
      -- Model Effects
      -- Model Contents
      (ers, fns) = modelFunc irs syms
  in (ers, (FuncType ExtrFn [] [] [] [] fnid'):fns)










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
