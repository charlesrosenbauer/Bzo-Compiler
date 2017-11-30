module IRModel where
import Data.Text
import Data.List
import Data.Map.Strict










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  attributes  :: AttrSet,
  funcNodes   :: [Node],
  funcId      :: FnId }

data FunctionKind = PureFn | ProcFn | ExtrFn

type FnId = Int










data TypeData = TypeData [([Int], TyId)]

type TyId = Int










data Attribute
  = IntAttribute AttrId Int
  | StrAttribute AttrId Text
  | BlAttribute  AttrId

type AttrId  = Int

type AttrSet = Map AttrId Attribute










data IRSymbols
  = IRSymbols{
    funcSymbols  :: Map Text   FnId,
    funcSymbols' :: Map FnId   Text,
    typeSymbols  :: Map Text   TyId,
    typeSymbols' :: Map TyId   Text,
    attrSymbols  :: Map Text   AttrId,
    attrSymbols' :: Map AttrId Text,
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










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | ICmpOp |   -- Integer Ops
                  UAddOp | USubOp | UMulOp | UDivOp | UModOp | UCmpOp |   -- Unsigned Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FCmpOp |   -- Float Ops
                  NAddOp | NSubOp | NMulOp | NDivOp | NModOp | NCmpOp |   -- Unum Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  CttzOp | CtlzOp | PCntOp | LShLOp | LShROp | AShROp

data HOFCode    = MapHF  | FoldHF | RedcHF | ZipHF  | UZipHF | ScanHF |
                  ChnHF  | FiltHF

data CondCode   = LSCond | GTCond | EQCond | NECond | LECond | GECond |
                  NZCond | EZCond
