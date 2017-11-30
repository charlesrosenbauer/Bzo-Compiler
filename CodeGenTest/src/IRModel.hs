module IRModel where
import Data.Text
import Data.List
import Data.Map.Strict










data FuncData = FuncType{
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  attributes  :: [Attribute],
  funcNodes   :: [Node],
  funcId      :: FnId }

type FnId = Int










data TypeData = TypeData [([Int], TyId)]

type TyId = Int










data Attribute
  = IntAttribute AttrId Int
  | StrAttribute AttrId Text
  | BlAttribute  AttrId

type AttrId = Int










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
  = CastNode  [Attribute]  Int  TypeData  Int
  | BinopNode [Attribute]  Int  BinopCode Int  Int
  | OpNode    [Attribute]  Int  OpCode    Int
  | OutNode   [Attribute]  Int  TypeData
  | InNode    [Attribute]  Int  TypeData
  | RetNode   [Attribute]  Int  Int       Int
  | CallNode  [Attribute] [Int] FnId     [Int]
  | HOFNode   [Attribute]  Int  HOFCode  [Int]
  | PhiNode   [Attribute]  Int  CondCode  Int  Int
  | FuncNode  [Attribute]  Int  FnId
  | TypeNode  [Attribute]  Int  TyId










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
