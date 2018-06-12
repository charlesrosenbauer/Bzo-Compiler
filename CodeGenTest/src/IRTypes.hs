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
  funcId      :: FnId }
  deriving Show

data FunctionKind = PureFn | ProcFn | ExtrFn
  deriving Show

type FnId = Int










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
