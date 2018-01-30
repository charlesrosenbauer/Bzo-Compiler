module IRModel where
import Data.Text
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
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
  | BinopNode AttrSet  Int  BinopCode Int  Int
  | OpNode    AttrSet  Int  OpCode    Int
  | ParNode   AttrSet  Int  TypeRef
  | RetNode   AttrSet  Int  TypeRef   Int
  | CallNode  AttrSet [Int] FnId     [Int]
  | HOFNode   AttrSet  Int  HOFCode  [Int]
  | PhiNode   AttrSet  Int  CondCode  Int  Int
  | FuncNode  AttrSet  Int  FnId
  | TypeNode  AttrSet  Int  TyId
  deriving Show










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp
                  deriving Show

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | ICmpOp |   -- Integer Ops
                  UAddOp | USubOp | UMulOp | UDivOp | UModOp | UCmpOp |   -- Unsigned Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FCmpOp |   -- Float Ops
                  NAddOp | NSubOp | NMulOp | NDivOp | NModOp | NCmpOp |   -- Unum Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  CttzOp | CtlzOp | PCntOp | LShLOp | LShROp | AShROp |
                  LogOp  | RootOp | ExpOp  | PowOp
                  deriving Show

data HOFCode    = MapHF  | FoldHF | RedcHF | ZipHF  | UZipHF | ScanHF |
                  ChnHF  | FiltHF
                  deriving Show

data CondCode   = LSCond | GTCond | EQCond | NECond | LECond | GECond |
                  NZCond | EZCond
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
  case (outs, unpack call, ins) of
    ([o],  "input", [ PI_Type p nms ty ])               -> typeLookup syms ty p (\x -> ParNode M.empty o (TypeRef nms x))
    ([o], "output", [(PI_Int _ n), (PI_Type p nms ty)]) -> typeLookup syms ty p (\x -> RetNode M.empty o (TypeRef nms x) n)
    ([o],   "iadd", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o IAddOp a b
    ([o],   "isub", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o ISubOp a b
    ([o],   "imul", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o IMulOp a b
    ([o],   "idiv", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o IDivOp a b
    ([o],   "imod", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o IModOp a b
    ([o],   "icmp", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o ICmpOp a b
    ([o],   "uadd", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o UAddOp a b
    ([o],   "usub", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o USubOp a b
    ([o],   "umul", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o UMulOp a b
    ([o],   "udiv", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o UDivOp a b
    ([o],   "umod", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o UModOp a b
    ([o],   "ucmp", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o UCmpOp a b
    ([o],   "fadd", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FAddOp a b
    ([o],   "fsub", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FSubOp a b
    ([o],   "fmul", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FMulOp a b
    ([o],   "fdiv", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FDivOp a b
    ([o],   "fmod", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FModOp a b
    ([o],   "fcmp", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o FCmpOp a b
    ([o],   "nadd", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NAddOp a b
    ([o],   "nsub", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NSubOp a b
    ([o],   "nmul", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NMulOp a b
    ([o],   "ndiv", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NDivOp a b
    ([o],   "nmod", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NModOp a b
    ([o],   "ncmp", [(PI_Int _ b), (PI_Int _ a)])       -> Right $ BinopNode M.empty o NCmpOp a b
    _  -> Left [IRErr p $ pack "Unrecognized node"]










typeLookup :: IRSymbols -> Text -> IRPos -> (TyId -> a) -> Either [IRErr] a
typeLookup (IRSymbols _ _ tys _ _ _ _ _ _) tyid p fn =
  case (M.lookup tyid tys) of
    Just x  -> Right $ fn x
    Nothing -> Left  [IRErr p $ append tyid $ pack " is not recognized as a defined type."]









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
