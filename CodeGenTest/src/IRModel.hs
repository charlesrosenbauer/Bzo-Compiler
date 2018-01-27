module IRModel where
import Data.Text
import Data.List as L
import Data.Map.Strict as M
import IRParser
import IRLexer










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  funchints   :: [Hint],
  funcattribs :: AttrSet,
  funcNodes   :: [Node],
  funcId      :: FnId }

data FunctionKind = PureFn | ProcFn | ExtrFn

type FnId = Int










data TypeData = TypeData{
  typeNodes   :: [([Int], TyId)],
  typehints   :: [Hint],
  typeattribs :: AttrSet }

type TyId = Int










data Attribute
  = IntAttribute AttrId Int
  | StrAttribute AttrId Text
  | BlAttribute  AttrId

type AttrId  = Int

type AttrSet = Map AttrId Attribute










data ConstantData
  = ConstString CnstId Text
  | ConstInt    CnstId Int
  | ConstBool   CnstId

type CnstId = Int










data Hint
  = HintInt Text Int
  | HintStr Text Text










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




















data Node
  = CastNode  AttrSet  Int  TypeData  Int
  | BinopNode AttrSet  Int  BinopCode Int  Int
  | OpNode    AttrSet  Int  OpCode    Int
  | ParNode   AttrSet  Int  TypeData
  | RetNode   AttrSet  Int  TypeData  Int
  | CallNode  AttrSet [Int] FnId     [Int]
  | HOFNode   AttrSet  Int  HOFCode  [Int]
  | PhiNode   AttrSet  Int  CondCode  Int  Int
  | FuncNode  AttrSet  Int  FnId
  | TypeNode  AttrSet  Int  TyId










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | ICmpOp |   -- Integer Ops
                  UAddOp | USubOp | UMulOp | UDivOp | UModOp | UCmpOp |   -- Unsigned Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FCmpOp |   -- Float Ops
                  NAddOp | NSubOp | NMulOp | NDivOp | NModOp | NCmpOp |   -- Unum Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  CttzOp | CtlzOp | PCntOp | LShLOp | LShROp | AShROp |
                  LogOp  | RootOp | ExpOp  | PowOp

data HOFCode    = MapHF  | FoldHF | RedcHF | ZipHF  | UZipHF | ScanHF |
                  ChnHF  | FiltHF

data CondCode   = LSCond | GTCond | EQCond | NECond | LECond | GECond |
                  NZCond | EZCond










modelIR :: [IRParseItem] -> ([IRErr], IRSymbols, Map FnId FuncData,
                                                 Map TyId TypeData,
                                                 Map AttrId Attribute,
                                                 Map CnstId ConstantData,
                                                 [Hint])
modelIR irs =
  let symbols = getSymbols irs
  in ([], symbols, M.empty, M.empty, M.empty, M.empty, [])










modelIRHelper :: IRSymbols -> [IRParseItem] -> ([IRErr], [FuncData], [TypeData], [Attribute], [ConstantData], [Hint]) -> ([IRErr], [FuncData], [TypeData], [Attribute], [ConstantData], [Hint])
modelIRHelper syms@(IRSymbols fs fs' ts ts' as as' cs cs' top) ((PI_FnDef p fnid typ nodes):irs) (errs, fns, tys, ats, cts, hts) =
  let (err0, fs'', fs''') = customInsert [IRErr p $ append fnid $ pack " defined multiple times."] fs fs' fnid (top+1)
      -- Add pass to model functions
  in modelIRHelper (IRSymbols fs'' fs''' ts ts' as as' cs cs' (top + 1)) irs (err0 ++ errs, fns, tys, ats, cts, hts)

modelIRHelper syms@(IRSymbols fs fs' ts ts' as as' cs cs' top) ((PI_TyDef p tyid sz nodes):irs) (errs, fns, tys, ats, cts, hts) =
  let (err0, ts'', ts''') = customInsert [IRErr p $ append tyid $ pack " defined multiple times."] ts ts' tyid (top+1)
      -- Add pass to model types
  in modelIRHelper (IRSymbols fs fs' ts'' ts''' as as' cs cs' (top + 1)) irs (err0 ++ errs, fns, tys, ats, cts, hts)

modelIRHelper syms [] ret = ret

modelIRHelper syms (item:irs) (errs, fns, tys, ats, cts, hts) = modelIRHelper syms irs ((IRErr (ppos item) $ pack "Unrecognized pattern."):errs, fns, tys, ats, cts, hts)










customInsert :: (Ord k, Ord a) => [IRErr] -> Map k a -> Map a k -> k -> a -> ([IRErr], Map k a, Map a k)
customInsert errs forward backward k a =
  case (M.lookup k forward) of
    Just x  -> (errs, forward,              backward             )
    Nothing -> ([]  , M.insert k a forward, M.insert a k backward)










getSymbols :: [IRParseItem] -> IRSymbols
getSymbols [] = (IRSymbols M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty 0)

getSymbols ((PI_Defs _ defs):irs) = getSymbols (irs ++ defs)

getSymbols ((PI_FnDef _ fnid _ _):irs) =
  let (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top) = getSymbols irs
      nfns' = M.insert (top + 1) fnid fns'
      nfns  = M.insert fnid (top + 1) fns
  in  (IRSymbols nfns nfns' tys tys' ats ats' cns cns' (top+1))

getSymbols ((PI_TyDef _ tyid _ _):irs) =
  let (IRSymbols fns fns'  tys  tys' ats ats' cns cns'  top) = getSymbols irs
      ntys' = M.insert (top + 1) tyid tys'
      ntys  = M.insert tyid (top + 1) tys
  in  (IRSymbols fns fns' ntys ntys' ats ats' cns cns' (top+1))

getSymbols ((PI_PrDef _ fnid _ _ _):irs) =
  let (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top) = getSymbols irs
      nfns' = M.insert (top + 1) fnid fns'
      nfns  = M.insert fnid (top + 1) fns
  in  (IRSymbols nfns nfns' tys tys' ats ats' cns cns' (top+1))

getSymbols ((PI_ExDef _ fnid _ _):irs) =
  let (IRSymbols  fns  fns' tys tys' ats ats' cns cns'  top) = getSymbols irs
      nfns' = M.insert (top + 1) fnid fns'
      nfns  = M.insert fnid (top + 1) fns
  in  (IRSymbols nfns nfns' tys tys' ats ats' cns cns' (top+1))

getSymbols ((PI_Const _ cid):irs) =
  let (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top) = getSymbols irs
      ncns' = M.insert (top + 1) cid cns'
      ncns  = M.insert cid (top + 1) cns
  in  (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1))

getSymbols ((PI_ConstInt _ cid _):irs) =
  let (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top) = getSymbols irs
      ncns' = M.insert (top + 1) cid cns'
      ncns  = M.insert cid (top + 1) cns
  in  (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1))

getSymbols ((PI_ConstStr _ cid _):irs) =
  let (IRSymbols fns fns' tys tys' ats ats'  cns  cns'  top) = getSymbols irs
      ncns' = M.insert (top + 1) cid cns'
      ncns  = M.insert cid (top + 1) cns
  in  (IRSymbols fns fns' tys tys' ats ats' ncns ncns' (top+1))

getSymbols ((PI_Attr _ aid):irs) =
  let (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top) = getSymbols irs
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
  in  (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1))

getSymbols ((PI_AttrInt _ aid _):irs) =
  let (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top) = getSymbols irs
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
  in  (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1))

getSymbols ((PI_AttrStr _ aid _):irs) =
  let (IRSymbols fns fns' tys tys'  ats  ats' cns cns'  top) = getSymbols irs
      nats' = M.insert (top + 1) aid ats'
      nats  = M.insert aid (top + 1) ats
  in  (IRSymbols fns fns' tys tys' nats nats' cns cns' (top+1))

getSymbols (_:irs) = getSymbols irs
