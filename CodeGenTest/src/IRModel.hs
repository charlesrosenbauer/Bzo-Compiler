module IRModel where
import Data.Text
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Either
import IRParser
import IRLexer









-- Change later and just import HigherOrder.hs
ife :: Bool -> a -> a -> a
ife True  a b = a
ife False a b = b










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  funchints   :: [Hint],
  funcattribs :: AttrSet,
  funcNodes   :: [Node],
  funcId      :: FnId }
  deriving Show

data FunctionKind = PureFn | ProcFn | ExtrFn
  deriving Show

type FnId = Int










data TypeData = TypeData{
  typeNodes   :: [([Int], TyId)],
  typehints   :: [Hint],
  typeattribs :: AttrSet }
  deriving Show

data TypeRef = TypeRef [Int] TyId
  deriving Show

type TyId = Int










data Attribute
  = IntAttribute AttrId Int
  | StrAttribute AttrId Text
  | BlAttribute  AttrId
  deriving Show

type AttrId  = Int

type AttrSet = Map AttrId Attribute










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
    attrSymbols  :: Map Text   AttrId,
    attrSymbols' :: Map AttrId Text,
    cnstSymbols  :: Map Text   CnstId,
    cnstSymbols' :: Map CnstId Text,
    idTop        :: Int }
    deriving Show




















data Node
  = CastNode  AttrSet  Int  TypeRef   Int
  | BinopNode AttrSet  Int  BinopCode Int   Int
  | OpNode    AttrSet  Int  OpCode    Int
  | ParNode   AttrSet  Int  TypeRef
  | RetNode   AttrSet  Int  TypeRef   Int
  | CallNode  AttrSet [Int] CallCode  FnId [Int]
  | HOFNode   AttrSet  Int  HOFCode  [Int]
  | PhiNode   AttrSet  Int  CondCode  Int   Int   Int
  | FuncNode  AttrSet  Int  FnId
  | TypeNode  AttrSet  Int  TyId
  deriving Show










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp| CttzOp | CtlzOp |
                  PCntOp
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










modelIR :: IRParseItem -> Either [IRErr] (IRSymbols, Map FnId FuncData,
                                                 Map TyId TypeData,
                                                 Map AttrId Attribute,
                                                 Map CnstId ConstantData,
                                                 [Hint])
modelIR irs =
  let (errs, symbols) = getSymbols [irs]
  in case errs of
      [] -> Right (symbols, M.empty, M.empty, M.empty, M.empty, [])
      er -> Left  er











modelNode :: IRSymbols -> IRParseItem -> Either [IRErr] Node
modelNode syms (PI_Node p outs call ins) =
  case (outs, unpack call, L.reverse ins) of
    ([o],  "input", [ PI_Type p nms ty ])               -> typeLookup syms ty p (\x -> ParNode  M.empty o (TypeRef nms x))
    ([o], "output", [(PI_Type p nms ty), (PI_Int _ n)]) -> typeLookup syms ty p (\x -> RetNode  M.empty o (TypeRef nms x) n)
    ([o],   "cast", [(PI_Type p nms ty), (PI_Int _ n)]) -> typeLookup syms ty p (\x -> CastNode M.empty o (TypeRef nms x) n)
    ([o],  "phiLS", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  LSCond a b c
    ([o],  "phiGT", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  GTCond a b c
    ([o],  "phiEQ", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  EQCond a b c
    ([o],  "phiNE", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  NECond a b c
    ([o],  "phiLE", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  LECond a b c
    ([o],  "phiGE", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  GECond a b c
    ([o],  "phiNZ", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  NZCond a b c
    ([o],  "phiEZ", [(PI_Int _ a), (PI_Int _ b), (PI_Int _ c)])             -> Right $ PhiNode  M.empty o  EZCond a b c
    ([o],   "iadd", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  IAddOp a b
    ([o],   "isub", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  ISubOp a b
    ([o],   "imul", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  IMulOp a b
    ([o],   "idiv", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  IDivOp a b
    ([o],   "imod", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  IModOp a b
    ([o],   "icmp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  ICmpOp a b
    ([o],   "uadd", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  UAddOp a b
    ([o],   "usub", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  USubOp a b
    ([o],   "umul", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  UMulOp a b
    ([o],   "udiv", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  UDivOp a b
    ([o],   "umod", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  UModOp a b
    ([o],   "ucmp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  UCmpOp a b
    ([o],   "fadd", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FAddOp a b
    ([o],   "fsub", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FSubOp a b
    ([o],   "fmul", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FMulOp a b
    ([o],   "fdiv", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FDivOp a b
    ([o],   "fmod", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FModOp a b
    ([o],   "fcmp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FCmpOp a b
    ([o],   "nadd", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NAddOp a b
    ([o],   "nsub", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NSubOp a b
    ([o],   "nmul", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NMulOp a b
    ([o],   "ndiv", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NDivOp a b
    ([o],   "nmod", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NModOp a b
    ([o],   "ncmp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NCmpOp a b
    ([o],     "or", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o    OrOp a b
    ([o],    "and", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o   AndOp a b
    ([o],    "xor", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o   XorOp a b
    ([o],    "lor", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o   LOrOp a b
    ([o],   "land", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  LAndOp a b
    ([o],   "lxor", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  LXorOp a b
    ([o],   "lshl", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  LShLOp a b
    ([o],   "lshr", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  LShROp a b
    ([o],   "ashr", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  AShROp a b
    ([o],   "nlog", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NLogOp a b
    ([o],  "nroot", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o NRootOp a b
    ([o],   "nexp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  NExpOp a b
    ([o],   "flog", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FLogOp a b
    ([o],  "froot", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o FRootOp a b
    ([o],   "fexp", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o  FExpOp a b
    ([o],    "pow", [(PI_Int _ a), (PI_Int _ b)])       -> Right $ BinopNode M.empty o   PowOp a b
    ([o],    "abs", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   AbsOp a
    ([o],   "trnc", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  TrncOp a
    ([o],   "wide", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   AbsOp a
    ([o],    "neg", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   NegOp a
    ([o],    "not", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   NotOp a
    ([o],   "lnot", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  LNotOp a
    ([o],   "sqrt", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  SqrtOp a
    ([o],   "cbrt", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  CbrtOp a
    ([o],    "lg2", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   Lg2Op a
    ([o],   "lg10", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  Lg10Op a
    ([o],    "sin", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   SinOp a
    ([o],    "cos", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   CosOp a
    ([o],    "tan", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o   TanOp a
    ([o],   "asin", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  AsinOp a
    ([o],   "acos", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  AcosOp a
    ([o],   "atan", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  AtanOp a
    ([o],   "sinh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  SinhOp a
    ([o],   "cosh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  CoshOp a
    ([o],   "tanh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  TanhOp a
    ([o],  "asinh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o AsinhOp a
    ([o],  "acosh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o AcoshOp a
    ([o],  "atanh", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o AtanhOp a
    ([o],   "cttz", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  CttzOp a
    ([o],   "ctlz", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  CtlzOp a
    ([o],   "pcnt", [(PI_Int _ a)])                     -> Right $ OpNode    M.empty o  PCntOp a
    ( os, "prcall", ((PI_Proc p fn): pars))         ->
      let nms = verifyAllNums [] pars
          fnc = funcLookup syms fn p (\fnid -> CallNode M.empty os PrCall fnid $ L.head $ rights [nms])
      in case nms of
          Left er -> Left [er]
          Right _ -> fnc
    ( os, "rccall", ((PI_Extn p fn): pars))         ->
      let nms = verifyAllNums [] pars
          fnc = funcLookup syms fn p (\fnid -> CallNode M.empty os ExCall fnid $ L.head $ rights [nms])
      in case nms of
          Left er -> Left [er]
          Right _ -> fnc
    ( os, fn, pars)         ->
      let nms = verifyAllNums [] pars
          fnc = funcLookup syms (pack fn) p (\fnid -> CallNode M.empty os FnCall fnid $ L.head $ rights [nms])
      in case nms of
          Left er -> Left [er]
          Right _ -> fnc

    _  -> Left [IRErr p $ pack "Unrecognized node"]










verifyAllNums :: [Int] -> [IRParseItem] -> Either IRErr [Int]
verifyAllNums nums ((PI_Int _ a):irs) = verifyAllNums (nums ++ [a]) irs
verifyAllNums nums []                 = Right  nums
verifyAllNums nums (ir:irs)           = Left $ IRErr (ppos ir) $ pack "Invalid parameter to function."










typeLookup :: IRSymbols -> Text -> IRPos -> (TyId -> a) -> Either [IRErr] a
typeLookup (IRSymbols _ _ tys _ _ _ _ _ _) tyid p fn =
  case (M.lookup tyid tys) of
    Just x  -> Right $ fn x
    Nothing -> Left  [IRErr p $ append tyid $ pack " is not recognized as a defined type."]










funcLookup :: IRSymbols -> Text -> IRPos -> (FnId -> a) -> Either [IRErr] a
funcLookup (IRSymbols fns _ _ _ _ _ _ _ _) fnid p fn =
  case (M.lookup fnid fns) of
    Just x  -> Right $ fn x
    Nothing -> Left  [IRErr p $ append fnid $ pack " is not recognized as a defined function."]









getSymbols :: [IRParseItem] -> ([IRErr], IRSymbols)
getSymbols [] = ([], (IRSymbols M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty 0))

getSymbols ((PI_Defs _ defs):irs) = getSymbols (irs ++ defs)

getSymbols ((PI_FnDef p fnid _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' ats ats' cns cns' top'))

getSymbols ((PI_TyDef p tyid _ _):irs) =
  let (errs, (IRSymbols fns fns'  tys  tys' ats ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup tyid tys) top (top+1)
      ntys' = M.insert top' tyid tys'
      ntys  = M.insert tyid top' tys
      errs' = ife (top /= top') errs ((IRErr p $ append tyid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' ntys ntys' ats ats' cns cns' (top+1)))

getSymbols ((PI_PrDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' ats ats' cns cns' (top+1)))

getSymbols ((PI_ExDef p fnid _ _ _ _):irs) =
  let (errs, (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup fnid fns) top (top+1)
      nfns' = M.insert top' fnid fns'
      nfns  = M.insert fnid top' fns
      errs' = ife (top /= top') errs ((IRErr p $ append fnid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols nfns nfns' tys tys' ats ats' cns cns' (top+1)))

getSymbols ((PI_Const p cid):irs) =
  let (errs, (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1)))

getSymbols ((PI_ConstInt p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1)))

getSymbols ((PI_ConstStr p cid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup cid cns) top (top+1)
      ncns' = M.insert top' cid cns'
      ncns  = M.insert cid top' cns
      errs' = ife (top /= top') errs ((IRErr p $ append cid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1)))

getSymbols ((PI_Attr p aid):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup aid ats) top (top+1)
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
      errs' = ife (top /= top') errs ((IRErr p $ append aid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1)))

getSymbols ((PI_AttrInt p aid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup aid ats) top (top+1)
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
      errs' = ife (top /= top') errs ((IRErr p $ append aid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1)))

getSymbols ((PI_AttrStr p aid _):irs) =
  let (errs, (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top)) = getSymbols irs
      top'  = ife (isJust $ M.lookup aid ats) top (top+1)
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
      errs' = ife (top /= top') errs ((IRErr p $ append aid $ pack " defined multiple times."):errs)
  in  (errs', (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1)))

getSymbols (_:irs) = getSymbols irs
