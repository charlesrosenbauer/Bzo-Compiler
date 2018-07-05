module IRTypes where
import Data.Text
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Tuple










data IRPos = IRPos{
  irLine   :: Int,
  irColumn :: Int,
  irFName  :: Text }
  deriving (Eq, Show)










data IRErr = IRErr IRPos Text deriving Eq
instance Show IRErr where
  show (IRErr (IRPos l c f) t) = "At " ++ (show l) ++ ":" ++ (show c) ++ ", in " ++ (show f) ++ ":\n" ++ unpack t










data IRToken
  = FuncToken   {tpos :: IRPos, ttxt :: Text}
  | NodeToken   {tpos :: IRPos, tnum :: Int }
  | TypeToken   {tpos :: IRPos, ttxt :: Text}
  | ExternToken {tpos :: IRPos, ttxt :: Text}
  | ProcToken   {tpos :: IRPos, ttxt :: Text}
  | ConstToken  {tpos :: IRPos, ttxt :: Text}
  | HintToken   {tpos :: IRPos, ttxt :: Text}
  | NumToken    {tpos :: IRPos, tnum :: Int }
  | StrToken    {tpos :: IRPos, ttxt :: Text}
  | ArrToken    {tpos :: IRPos, tnum :: Int }
  | AttribToken {tpos :: IRPos, ttxt :: Text}
  | OpenBrace   {tpos :: IRPos}
  | CloseBrace  {tpos :: IRPos}
  | OpenParen   {tpos :: IRPos}
  | CloseParen  {tpos :: IRPos}
  | OpenBox     {tpos :: IRPos}
  | CloseBox    {tpos :: IRPos}
  | FTypeToken  {tpos :: IRPos}
  | EffectToken {tpos :: IRPos}
  | DefFunc     {tpos :: IRPos}
  | DefType     {tpos :: IRPos}
  | DefExtern   {tpos :: IRPos}
  | DefProc     {tpos :: IRPos}
  | NewLine     {tpos :: IRPos}
  | PtrToken    {tpos :: IRPos}
  | NilToken
  deriving (Eq, Show)










data IRParseItem
  = PI_FnHeader {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem}
  | PI_FnDef    {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem, pars :: [IRParseItem]}
  | PI_TyHeader {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_TyDef    {ppos :: IRPos, txt0 :: Text, num0 :: Int, pars :: [IRParseItem]}
  | PI_PrHeader {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem, pars0 :: [IRParseItem], pars1 :: [IRParseItem]}
  | PI_PrDef    {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem, pars0 :: [IRParseItem], pars1 :: [IRParseItem], pars :: [IRParseItem]}
  | PI_ExHeader {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem, pars0 :: [IRParseItem], pars1 :: [IRParseItem]}
  | PI_ExDef    {ppos :: IRPos, txt0 :: Text, par0 :: IRParseItem, pars0 :: [IRParseItem], pars1 :: [IRParseItem], pars :: [IRParseItem]}
  | PI_NL       {ppos :: IRPos}
  | PI_Arr      {ppos :: IRPos, num0 :: Int}
  | PI_FTyPart0 {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_FTyPart1 {ppos :: IRPos, pars0:: [IRParseItem], pars1:: [IRParseItem]}
  | PI_FTy      {ppos :: IRPos, pars0:: [IRParseItem], pars1:: [IRParseItem]}
  | PI_EffPart0 {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_EffPart1 {ppos :: IRPos, pars0:: [IRParseItem], pars1:: [IRParseItem]}
  | PI_Effects  {ppos :: IRPos, pars0:: [IRParseItem], pars1:: [IRParseItem]}
  | PI_Type     {ppos :: IRPos, nums :: [Int], txt0 :: Text}
  | PI_Node     {ppos :: IRPos, nums :: [Int], txt0 :: Text, pars :: [IRParseItem]}
  | PI_NS       {ppos :: IRPos, nums :: [Int]}
  | PI_Nodes    {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Defs     {ppos :: IRPos, pars :: [IRParseItem]}
  | PI_Const    {ppos :: IRPos, txt0 :: Text}
  | PI_ConstInt {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_ConstStr {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_HintInt  {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_HintStr  {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_AttrStr  {ppos :: IRPos, txt0 :: Text, txt1 :: Text}
  | PI_AttrInt  {ppos :: IRPos, txt0 :: Text, num0 :: Int}
  | PI_Attr     {ppos :: IRPos, txt0 :: Text}
  | PI_Str      {ppos :: IRPos, txt0 :: Text}
  | PI_Int      {ppos :: IRPos, num0 :: Int}
  | PI_Func     {ppos :: IRPos, txt0 :: Text}
  | PI_Proc     {ppos :: IRPos, txt0 :: Text}
  | PI_Extn     {ppos :: IRPos, txt0 :: Text}
  | PI_Token    {ppos :: IRPos, tkn :: IRToken}
  deriving (Eq, Show)










data FuncData = FuncType{
  funcKind    :: FunctionKind,
  inputTypes  :: [TypeData],
  outputTypes :: [TypeData],
  readEffects :: [TyId],
  writeEffects:: [TyId],
  funchints   :: [Hint],
  funcNodes   :: [Node],
  funcId      :: FnId,
  fnNodes     :: Map Int Node,
  fnFromOuts  :: Map Int Int,
  nodeCount   :: Int }
  deriving Show

data FunctionKind = PureFn | ProcFn | ExtrFn
  deriving Show

type FnId = Int










data Node
  = CastNode   { nout:: Int , ntyp:: TypeRef,   npar:: Int }
  | ConstNode  { nout:: Int , ntyp:: TypeRef,   csid:: CnstId }
  | GetNode    { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int }
  | SetNode    { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int, npr2::Int }
  | LoadNode   { nout:: Int , ntyp:: TypeRef,   npar:: Int }
  | StoreNode  { nout:: Int , ntyp:: TypeRef,   npr0:: Int , npr1:: Int }
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










nodeOuts :: Node -> [Int]
nodeOuts (CallNode ots _ _ _ _  ) = ots
nodeOuts (HOFNode  ots _ _ _ _  ) = ots
nodeOuts (PhiNode  ots _ _ _ _ _) = ots
nodeOuts nd = [nout nd]










getIns  :: Node -> [Int]
getIns  (SetNode   _ _     p0 p1 _)  = [p0, p1]     -- The last element references type-local variables, not scope-local ones
getIns  (GetNode   _ _     p0 _   )  = [p0]         --   "
getIns  (StoreNode _ _     p0 p1   ) = [p0, p1]
getIns  (BinopNode _ _ _   p0 p1   ) = [p0, p1]
getIns  (TrinopNode  _ _ _ p0 p1 p2) = [p0, p1, p2]
getIns  (ParNode     _ _)            = []
getIns  (ConstNode _ _ _)            = []
getIns  (CallNode  _ _ _ _ ps)       = ps
getIns  (HOFNode   _ _ _ ps _)       = ps
getIns  (PhiNode   _ _ p0 _ _ ps)    = (p0:ps)
getIns  (CondNode  _ _ p0 p1)        = [p0, p1]
getIns  node                         = [npar node]










data OpCode     = AbsOp  | TrncOp | WideOp | NegOp  | NotOp  | LNotOp |
                  SqrtOp | CbrtOp | Lg2Op  | Lg10Op | SinOp  | CosOp  |
                  TanOp  | AsinOp | AcosOp | AtanOp | SinhOp | CoshOp |
                  TanhOp | AsinhOp| AcoshOp| AtanhOp| CttzOp | CtlzOp |
                  CeilOp | FloorOp| RoundOp|
                  PCntOp | BRevsOp| BSwapOp| LSBFillOp | LCBFillOp |
                  CnstOp
                  deriving Show

data BinopCode  = IAddOp | ISubOp | IMulOp | IDivOp | IModOp | IMinOp | IMaxOp |  -- Integer Ops
                  FAddOp | FSubOp | FMulOp | FDivOp | FModOp | FMinOp | FMaxOp |  -- Float Ops
                  OrOp   | AndOp  | XorOp  | LOrOp  | LAndOp | LXorOp |
                  LShLOp | LShROp | AShROp | RRotOp | LRotOp | BDepOp | BExtOp |
                  FLogOp | FRootOp| FExpOp | PowOp
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










intCompatOp :: OpCode -> Bool
intCompatOp AbsOp     = True
intCompatOp TrncOp    = True
intCompatOp WideOp    = True
intCompatOp NegOp     = True
intCompatOp NotOp     = True
intCompatOp Lg2Op     = True
intCompatOp CttzOp    = True
intCompatOp CtlzOp    = True
intCompatOp PCntOp    = True
intCompatOp BRevsOp   = True
intCompatOp BSwapOp   = True
intCompatOp LSBFillOp = True
intCompatOp LCBFillOp = True
intCompatOp CnstOp    = True
intCompatOp _         = False










intCompatBinop :: BinopCode -> Bool
intCompatBinop IAddOp   = True
intCompatBinop ISubOp   = True
intCompatBinop IMulOp   = True
intCompatBinop IDivOp   = True
intCompatBinop IModOp   = True
intCompatBinop IMinOp   = True
intCompatBinop IMaxOp   = True
intCompatBinop OrOp     = True
intCompatBinop AndOp    = True
intCompatBinop XorOp    = True
intCompatBinop LShLOp   = True
intCompatBinop LShROp   = True
intCompatBinop AShROp   = True
intCompatBinop RRotOp   = True
intCompatBinop LRotOp   = True
intCompatBinop BDepOp   = True
intCompatBinop BExtOp   = True
intCompatBinop PowOp    = True
intCompatBinop _        = False










intCompatTrinop :: TrinopCode -> Bool
intCompatTrinop IFMAOp = True
intCompatTrinop IFMSOp = True
intCompatTrinop _      = False










fltCompatOp :: OpCode -> Bool
fltCompatOp AbsOp     = True
fltCompatOp TrncOp    = True
fltCompatOp WideOp    = True
fltCompatOp NegOp     = True
fltCompatOp Lg2Op     = True
fltCompatOp Lg10Op    = True
fltCompatOp SinOp     = True
fltCompatOp CosOp     = True
fltCompatOp TanOp     = True
fltCompatOp AsinOp    = True
fltCompatOp AcosOp    = True
fltCompatOp AtanOp    = True
fltCompatOp SinhOp    = True
fltCompatOp CoshOp    = True
fltCompatOp TanhOp    = True
fltCompatOp AsinhOp   = True
fltCompatOp AcoshOp   = True
fltCompatOp AtanhOp   = True
fltCompatOp CeilOp    = True
fltCompatOp FloorOp   = True
fltCompatOp RoundOp   = True
fltCompatOp CnstOp    = True
fltCompatOp _         = False










fltCompatBinop :: BinopCode -> Bool
fltCompatBinop FAddOp   = True
fltCompatBinop FSubOp   = True
fltCompatBinop FMulOp   = True
fltCompatBinop FDivOp   = True
fltCompatBinop FModOp   = True
fltCompatBinop FMinOp   = True
fltCompatBinop FMaxOp   = True
fltCompatBinop FLogOp   = True
fltCompatBinop FRootOp  = True
fltCompatBinop FExpOp   = True
fltCompatBinop PowOp    = True
fltCompatBinop _        = False










fltCompatTrinop :: TrinopCode -> Bool
fltCompatTrinop FFMAOp = True
fltCompatTrinop FFMSOp = True
fltCompatTrinop _      = False










isIntType :: TypeRef -> Bool
isIntType ty
  | ty == typeI8  = True
  | ty == typeI16 = True
  | ty == typeI32 = True
  | ty == typeI64 = True
  | ty == typeU8  = True
  | ty == typeU16 = True
  | ty == typeU32 = True
  | ty == typeU64 = True
  | otherwise     = False










isFltType :: TypeRef -> Bool
isFltType ty
  | ty == typeF16 = True
  | ty == typeF32 = True
  | ty == typeF64 = True
  | otherwise     = False










isUnmType :: TypeRef -> Bool
isUnmType ty
  | ty == typeN16 = True
  | ty == typeN32 = True
  | ty == typeN64 = True
  | otherwise     = False










data TypeData = TypeData{
  typeKind    :: TypeKind,
  typeNodes   :: [TypeNode],
  typeSize    :: Int,
  typehints   :: [Hint],
  typeId      :: TyId }
  deriving (Show, Eq)

data TypeRef = TypeRef [Int] TyId | UndefType
  deriving (Show, Eq)

data TypeKind = PureTy | FXTy_SRSW | FXTy_PRSW | FXTy_SRPW | FXTy_PRPW
  deriving (Show, Eq)

type TyId = Int










data TypeNode
  = ElemNode      { tidx :: Int, tbyt :: Int,      ttyp :: TypeRef }
  | Contain1Node  { tidx :: Int, tcn1 :: Contain1, tty0 :: TypeRef }
  | Contain2Node  { tidx :: Int, tcn2 :: Contain2, tty0 :: TypeRef, tty1 :: TypeRef }
  | ImplNode      { tidx :: Int, timp :: ImplCode, tfnc :: FnId }
  deriving (Show, Eq)










data Contain1 = ListCont | SetCont  | BSetCont | RRBCont  |
                BoxCont  | QCont    | StkCont  | HeapCont
                deriving (Show, Eq)

data Contain2 = DictCont | HMapCont | AVLCont
                deriving (Show, Eq)

data ImplCode = ImplMap  | ImplSplit| ImplSFold| ImplPFold|
                ImplSScan| ImplPScan| ImplZip  | ImplUZip |
                ImplNext | ImplIndex| ImplHash | ImplEq   |
                ImplCmp  | ImplSerl | ImplDSerl| ImplSize
                deriving (Show, Eq)










typeI8    = TypeRef [] 1
typeI16   = TypeRef [] 2
typeI32   = TypeRef [] 3
typeI64   = TypeRef [] 4
typeU8    = TypeRef [] 5
typeU16   = TypeRef [] 6
typeU32   = TypeRef [] 7
typeU64   = TypeRef [] 8
typeN16   = TypeRef [] 9
typeN32   = TypeRef [] 10
typeN64   = TypeRef [] 11
typeF16   = TypeRef [] 12
typeF32   = TypeRef [] 13
typeF64   = TypeRef [] 14
typeASCII = TypeRef [] 15
typeUTF8  = TypeRef [] 16
typeUTF16 = TypeRef [] 17
typeUTF32 = TypeRef [] 18
typeChar  = TypeRef [] 19
typeUChar = TypeRef [] 20
typeBool  = TypeRef [] 21
typeI8v2  = TypeRef [] 22
typeI16v2 = TypeRef [] 23
typeI32v2 = TypeRef [] 24
typeI64v2 = TypeRef [] 25
typeU8v2  = TypeRef [] 26
typeU16v2 = TypeRef [] 27
typeU32v2 = TypeRef [] 28
typeU64v2 = TypeRef [] 29
typeN16v2 = TypeRef [] 30
typeN32v2 = TypeRef [] 31
typeN64v2 = TypeRef [] 32
typeF16v2 = TypeRef [] 33
typeF32v2 = TypeRef [] 34
typeF64v2 = TypeRef [] 35
typeI8v4  = TypeRef [] 36
typeI16v4 = TypeRef [] 37
typeI32v4 = TypeRef [] 38
typeI64v4 = TypeRef [] 39
typeU8v4  = TypeRef [] 40
typeU16v4 = TypeRef [] 41
typeU32v4 = TypeRef [] 42
typeU64v4 = TypeRef [] 43
typeN16v4 = TypeRef [] 44
typeN32v4 = TypeRef [] 45
typeN64v4 = TypeRef [] 46
typeF16v4 = TypeRef [] 47
typeF32v4 = TypeRef [] 48
typeF64v4 = TypeRef [] 49
typeI8v8  = TypeRef [] 50
typeI16v8 = TypeRef [] 51
typeI32v8 = TypeRef [] 52
typeI64v8 = TypeRef [] 53
typeU8v8  = TypeRef [] 54
typeU16v8 = TypeRef [] 55
typeU32v8 = TypeRef [] 56
typeU64v8 = TypeRef [] 57
typeN16v8 = TypeRef [] 58
typeN32v8 = TypeRef [] 59
typeN64v8 = TypeRef [] 60
typeF16v8 = TypeRef [] 61
typeF32v8 = TypeRef [] 62
typeF64v8 = TypeRef [] 63
typeI8v16 = TypeRef [] 64
typeI16v16= TypeRef [] 65
typeI32v16= TypeRef [] 66
typeI64v16= TypeRef [] 67
typeU8v16 = TypeRef [] 68
typeU16v16= TypeRef [] 69
typeU32v16= TypeRef [] 70
typeU64v16= TypeRef [] 71
typeN16v16= TypeRef [] 72
typeN32v16= TypeRef [] 73
typeN64v16= TypeRef [] 74
typeF16v16= TypeRef [] 75
typeF32v16= TypeRef [] 76
typeF64v16= TypeRef [] 77











data ConstantData
  = ConstStr  CnstId Text
  | ConstInt  CnstId Int
  | ConstBool CnstId      -- Probably going to remove this later
  deriving Show

type CnstId = Int










data Hint
  = HintInt Text Int
  | HintStr Text Text
  deriving (Show, Eq)










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
