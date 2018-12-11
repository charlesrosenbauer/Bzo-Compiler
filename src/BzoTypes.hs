module BzoTypes where
import Data.Int
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import HigherOrder










data BzoToken
  = TkStartTup     { spos :: !BzoPos }
  | TkEndTup       { spos :: !BzoPos }
  | TkStartDat     { spos :: !BzoPos }
  | TkEndDat       { spos :: !BzoPos }
  | TkStartDo      { spos :: !BzoPos }
  | TkEndDo        { spos :: !BzoPos }
  | TkSepExpr      { spos :: !BzoPos }
  | TkSepPoly      { spos :: !BzoPos }
  | TkCurrySym     { spos :: !BzoPos }
  | TkFilterSym    { spos :: !BzoPos }
  | TkLambdaSym    { spos :: !BzoPos }
  | TkReference    { spos :: !BzoPos }
  | TkWildcard     { spos :: !BzoPos }
  | TkDefine       { spos :: !BzoPos }
  | TkFnSym        { spos :: !BzoPos }
  | TkTupEmpt      { spos :: !BzoPos }
  | TkArrGnrl      { spos :: !BzoPos }
  | TkArrMod       { spos :: !BzoPos }
  | TkInt          { spos :: !BzoPos, valInt :: !Integer }
  | TkFlt          { spos :: !BzoPos, valFlt :: !Double  }
  | TkStr          { spos :: !BzoPos, valStr :: !T.Text  }
  | TkId           { spos :: !BzoPos, valId  :: !T.Text  }
  | TkTypeId       { spos :: !BzoPos, valId  :: !T.Text  }
  | TkMutId        { spos :: !BzoPos, valId  :: !T.Text  }
  | TkTyVar        { spos :: !BzoPos, valId  :: !T.Text  }
  | TkNewline      { spos :: !BzoPos }
  | TkBuiltin      { spos :: !BzoPos, valId  :: !T.Text  }
  | TkBIType       { spos :: !BzoPos, valId  :: !T.Text  }
  | TkNil
  deriving Eq










data BzoErr = Other
  | StringErr { position::BzoPos, errorStr::T.Text }
  | LexErr    { position::BzoPos, errorStr::T.Text }
  | ParseErr  { position::BzoPos, errorStr::T.Text }
  | TypeErr   { position::BzoPos, errorStr::T.Text }
  | SntxErr   { position::BzoPos, errorStr::T.Text }
  | DepErr    { errorStr::T.Text }
  | ParamErr  { errorStr::T.Text }
  | CfgErr    { errorStr::T.Text }
  | ModelErr  { position::BzoPos, errorStr::T.Text }
  | PrepErr   { position::BzoPos, errorStr::T.Text}










showBzErr :: BzoErr -> String
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ (show st)
showBzErr (SntxErr    p st) = "Syntax Error:\n " ++ (showErrPos p) ++ (show st)
showBzErr (DepErr       st) = "Dependency Error:\n" ++ (show st)
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ (show st)
showBzErr (CfgErr       st) = "Configuration Error:\n" ++ (show st)
showBzErr (PrepErr    p st) = "Preprocessor Error:\n" ++ (showErrPos p) ++ (show st)
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (show $ fileName p) ++ "\", at line " ++ (show $ line p) ++ ", column " ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: !Int,
  column   :: !Int,
  fileName :: !T.Text }
  deriving (Eq, Show)

compareBzoPos :: BzoPos -> BzoPos -> Ordering
compareBzoPos (BzoPos l0 c0 f0) (BzoPos l1 c1 f1)
  | (f0 == f1) && (l0 == l1) && (c0 == c1) = EQ
  | (f0 == f1) && (l0 == l1)               = compare c0 c1
  | (f0 == f1)                             = compare l0 l1
  | otherwise                              = compare f0 f1

instance Ord BzoPos where compare = compareBzoPos










showTk :: BzoToken -> String
showTk (TkStartTup      _) = "("
showTk (TkEndTup        _) = ")"
showTk (TkStartDat      _) = "["
showTk (TkEndDat        _) = "]"
showTk (TkStartDo       _) = "{"
showTk (TkEndDo         _) = "}"
showTk (TkSepExpr       _) = "."
showTk (TkSepPoly       _) = ","
showTk (TkCurrySym      _) = "`"
showTk (TkFilterSym     _) = ":"
showTk (TkLambdaSym     _) = ";"
showTk (TkReference     _) = "@"
showTk (TkWildcard      _) = "_"
showTk (TkDefine        _) = "::"
showTk (TkFnSym         _) = ";;"
showTk (TkTupEmpt       _) = "()"
showTk (TkArrGnrl       _) = "[]"
showTk (TkArrMod        _) = ".."
showTk (TkInt        _  x) = "I:"   ++ show x
showTk (TkFlt        _  x) = "F:"   ++ show x
showTk (TkStr        _ st) = "S:"   ++ show st
showTk (TkId         _ st) = "ID:"  ++ show st
showTk (TkMutId      _ st) = "MID:" ++ show st
showTk (TkTypeId     _ st) = "TID:" ++ show st
showTk (TkNewline       _) = "NEWL\n"
showTk (TkBuiltin    _ st) = "BI:"  ++ show st
showTk (TkBIType     _ st) = "BIT:" ++ show st
showTk (TkTyVar      _ st) = "TyVr:" ++ show st
showTk _                   = "NIL"
instance Show BzoToken where show = showTk










showTokens :: [BzoToken] -> String
showTokens tk = Prelude.unwords $ Prelude.map showTk tk










data BzoSyntax

    -- | Primitive Nodes
    = BzS_Int {
        pos       :: !BzoPos,
        sint      :: !Integer }
    | BzS_Flt {
        pos       :: !BzoPos,
        sflt      :: !Double }
    | BzS_Str {
        pos       :: !BzoPos,
        sstr      :: !T.Text }
    | BzS_Id {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_TyId {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_TyVar {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_MId {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_BId {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_BTId {
        pos       :: !BzoPos,
        sid       :: !T.Text }
    | BzS_Wildcard {
        pos       :: !BzoPos }
    | BzS_ExTypObj {
        pos       :: !BzoPos,
        sid       :: !T.Text,
        namespace :: !T.Text }
    | BzS_ExFunObj {
        pos       :: !BzoPos,
        sid       :: !T.Text,
        namespace :: !T.Text }
    | BzS_Nil {
        pos       :: !BzoPos }

    -- | Complex nodes
    | BzS_FilterObj {
        pos     :: !BzoPos,
        bzobj   :: !BzoSyntax,
        filts   :: ![BzoSyntax] }
    | BzS_CurryObj {
        pos     :: !BzoPos,
        bzobj   :: !BzoSyntax,
        crypars :: ![BzoSyntax] }
    | BzS_MapObj {
        pos     :: !BzoPos,
        bzobj   :: !BzoSyntax }
    | BzS_Statement {
        pos     :: !BzoPos,
        expr    :: !BzoSyntax }
    | BzS_ArrayObj{
        pos     :: !BzoPos,
        sint    :: !Integer,
        typ     :: !BzoSyntax }
    | BzS_LispCall {
        pos     :: !BzoPos,
        fncall  :: !BzoSyntax,
        exprs   :: ![BzoSyntax] }
    | BzS_Lambda {
        pos     :: !BzoPos,
        pars    :: !BzoSyntax,
        def     :: !BzoSyntax }
    | BzS_Expr {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }
    | BzS_Poly {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }
    | BzS_Cmpd {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }
    | BzS_FnTy {
        pos     :: !BzoPos,
        tyIn    :: !BzoSyntax,
        tyEx    :: !BzoSyntax }
    | BzS_Block {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }

    -- | Definitions
    | BzS_FunDef {
        pos    :: !BzoPos,
        inpars :: !BzoSyntax,
        fnid   :: !T.Text,
        expars :: !BzoSyntax,
        def    :: !BzoSyntax }
    | BzS_TypDef {
        pos    :: !BzoPos,
        pars   :: !BzoSyntax,
        tyid   :: !T.Text,
        typ    :: !BzoSyntax }
    | BzS_FnTypeDef {
        pos    :: !BzoPos,
        pars   :: !BzoSyntax,
        fnid   :: !T.Text,
        def    :: !BzoSyntax }
    | BzS_TyClassDef {
        pos    :: !BzoPos,
        pars   :: !BzoSyntax,
        tyid   :: !T.Text,
        defs   :: ![BzoSyntax] }
    | BzS_Calls {
        pos    :: !BzoPos,
        calls  :: ![BzoSyntax] }

    -- | Parser control nodes. These shouldn't show up after parsing
    | BzS_Namespace{
        pos     :: !BzoPos,
        sid     :: !T.Text }
    | BzS_MapMod {
        pos     :: !BzoPos }
    | BzS_Token {
        pos     :: !BzoPos,
        tok     :: !BzoToken }
    | BzS_CmpdHead {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }
    | BzS_PolyHead {
        pos     :: !BzoPos,
        exprs   :: ![BzoSyntax] }
    | BzS_BlockHead {
        pos     :: !BzoPos,
        sttmnts :: ![BzoSyntax] }
    | BzS_LispHead {
        pos     :: !BzoPos,
        fncall  :: !BzoSyntax,
        exprs   :: ![BzoSyntax] }
    | BzS_FnHead {
        pos     :: !BzoPos,
        inpars  :: !BzoSyntax,
        fnid    :: !T.Text,
        expars  :: !BzoSyntax }
    | BzS_TyHead {
        pos     :: !BzoPos,
        pars    :: !BzoSyntax,
        tyid    :: !T.Text }
    | BzS_TyClassHead {
        pos     :: !BzoPos,
        pars    :: !BzoSyntax,
        tyid    :: !T.Text,
        defs    :: ![BzoSyntax] }
    | BzS_Curry {
        pos     :: !BzoPos,
        obj     :: !BzoSyntax }
    | BzS_Filter {
        pos     :: !BzoPos,
        filt    :: !BzoSyntax }
    | BzS_ArrGenMod{
        pos     :: !BzoPos }
    | BzS_ArrHead{
        pos     :: !BzoPos,
        sint    :: !Integer }
    | BzS_Undefined{
        pos     :: !BzoPos }

    -- | Header and hint stuff
    | BzS_Import {
        pos      :: !BzoPos,
        fname    :: !T.Text,
        frename  :: !T.Text }
    | BzS_Include {
        pos      :: !BzoPos,
        fname    :: !T.Text,
        frename  :: !T.Text }
    | BzS_TyHint {
        pos      :: !BzoPos,
        inpars   :: !BzoSyntax,
        htid     :: !T.Text,
        expars   :: !BzoSyntax }
    | BzS_FnHint {
        pos      :: !BzoPos,
        inpars   :: !BzoSyntax,
        htid     :: !T.Text,
        expars   :: !BzoSyntax }
    | BzS_File {
        pos      :: !BzoPos,
        mname    :: !T.Text,
        fname    :: !T.Text,
        includes :: ![BzoSyntax],
        imports  :: ![BzoSyntax],
        defs     :: ![BzoSyntax] }

    deriving Eq










data CfgSyntax
  = LibLine {
      cpos    :: BzoPos,
      libName :: T.Text,
      libPath :: FilePath }
  | LibLines {
      cpos     :: BzoPos,
      libLines :: [CfgSyntax] }
  | LibParseItem {
      cpos     :: BzoPos,
      libtoken :: BzoToken }
  deriving (Eq, Show)









showAST :: BzoSyntax -> String
showAST (BzS_FunDef _ inpar fid expar def) = " {FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "} \n"
showAST (BzS_TypDef _ par tid def)         = " {TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "} \n"
showAST (BzS_FnTypeDef _ par fid def)      = " {FTDEF: " ++ (show fid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "} \n"
showAST (BzS_TyClassDef _ par tid def)     = " {TYCLASSDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (concatMap (\x -> "\n\n  " ++ show x) def) ++ "} \n"
showAST (BzS_Lambda _ par def)             = " {LAMBDA: " ++ (show par) ++ " :: " ++ (show def) ++ "} "
showAST (BzS_FnTy _ tin tex)               = " {FNTY: " ++ (show tin) ++ " ;;" ++ (show tex) ++ "} "
showAST (BzS_Filter _ filt)                = " {FILTER: " ++ (show filt) ++ "} "
showAST (BzS_Curry _ o)                    = " {CURRY: " ++ (show o) ++ "} "
showAST (BzS_Id _ i)                       = " ID: " ++ (show i)
showAST (BzS_MId _ i)                      = " MID: " ++ (show i)
showAST (BzS_TyId _ i)                     = " TID: " ++ (show i)
showAST (BzS_BId _ i)                      = " BID: " ++ (show i)
showAST (BzS_BTId _ i)                     = " BTID: " ++ (show i)
showAST (BzS_Namespace _ i)                = " {@ " ++ (show i) ++ "} "
showAST (BzS_Int _ i)                      = " INT: " ++ (show i)
showAST (BzS_Flt _ f)                      = " FLT: " ++ (show f)
showAST (BzS_Str _ s)                      = " STR: " ++ (show s)
showAST (BzS_Poly _ p)                     = " {POLY: " ++ (L.concat $ L.intersperse " , " $ L.reverse $ Prelude.map showAST p) ++ "} "
showAST (BzS_Cmpd _ c)                     = " {CMPD: " ++ (L.concat $ L.intersperse " . " $ L.reverse $ Prelude.map showAST c) ++ "} "
showAST (BzS_Block _ ex)                   = " {BK: " ++ (Prelude.concatMap showAST ex) ++ " } "
showAST (BzS_Expr _ ex)                    = " (EX: " ++ (L.concat $ L.intersperse " >> " $ L.reverse (Prelude.map showAST ex)) ++ " ) "
showAST (BzS_Statement _ ex)               = " (STMT: " ++ (showAST ex) ++ " ) "
showAST (BzS_LispCall _ fn ps)             = " (LISP: " ++ (show fn) ++ " <- " ++ (L.concat $ L.intersperse " , " $ L.reverse $ Prelude.map showAST ps) ++ " ) "
showAST (BzS_Calls _ c)                    = Prelude.concatMap (\s -> " CALL:: " ++ (show s) ++ "\n") c
showAST (BzS_Wildcard _)                   = " _ "
showAST (BzS_MapMod _)                     = " .. "
showAST (BzS_ArrGenMod _)                  = " [] "
showAST (BzS_ArrayObj _ sz ex)             = " [ " ++ (show sz) ++ ":" ++ (show ex) ++ " ] "
showAST (BzS_Nil _)                        = " () "
showAST (BzS_TyVar _ i)                    = " TyVr: " ++ (show i)
showAST (BzS_ExTypObj _ o n)               = " <" ++ (show o) ++ " from " ++ (show n) ++ "> "
showAST (BzS_ExFunObj _ o n)               = " <" ++ (show o) ++ " from " ++ (show n) ++ "> "
showAST (BzS_FilterObj _ o f)              = " <" ++ (show o) ++ " of type " ++ (show f) ++ "> "
showAST (BzS_CurryObj  _ o p)              = " <" ++ (show $ L.reverse p) ++ " applied to " ++ (show o) ++ "> "
showAST (BzS_MapObj    _ o)                = " <" ++ (show o) ++ " .. > "
showAST (BzS_Token     _ t)                = (show t)
showAST (BzS_TyHint    _ ins h exs)        = " {Ty HINT: " ++ (show ins) ++ " -> " ++ (show h) ++ " -> " ++ (show exs) ++ "} \n"
showAST (BzS_FnHint    _ ins h exs)        = " {Fn HINT: " ++ (show ins) ++ " -> " ++ (show h) ++ " -> " ++ (show exs) ++ "} \n"

showAST (BzS_Import    _ fn fr)            = "    -- Import  " ++ (show fn) ++ " as " ++ (show fr) ++ " --\n"
showAST (BzS_Include   _ fn fr)            = "    -- Include " ++ (show fn) ++ " as " ++ (show fr) ++ " --\n"
showAST (BzS_File      _ mn fn ins ims dfs)= "\n-- (File, Module): (" ++ (show fn) ++ ", " ++ (show mn) ++ ")\nIncludes:\n" ++ (concatMap show ins) ++ "\nImports:\n" ++ (concatMap show ims) ++ "\nDefs:\n" ++ (concatMap show dfs)

showAST (BzS_Undefined _)                  = " UNDEFINED "

showAST (BzS_BlockHead _ _)                = "BLKHEAD "
showAST (BzS_CmpdHead  _ _)                = "CMPDHEAD "
showAST (BzS_PolyHead  _ _)                = "POLYHEAD "
showAST (BzS_FnHead _ _ _ _)               = "FNHEAD "
showAST (BzS_LispHead _ _ _)               = "LSPHEAD "
showAST (BzS_ArrHead  _ _)                 = "ARRHEAD "
showAST _                                  = " <???> "
instance Show BzoSyntax where show = showAST










data (Show a) => BzoFileModel a
  = BzoFileModel{
      bfm_moduleName    :: !T.Text,
      bfm_filepath      :: !FilePath,
      bfm_domain        :: !T.Text,
      bfm_fileModel     :: !a,
      bfm_fileImports   :: ![T.Text],
      bfm_fileLinks     :: ![T.Text],
      bfm_fileImportsAs :: ![(T.Text, T.Text)],   -- [(import fst, refer to as snd)]
      bfm_fileLinksAs   :: ![(T.Text, T.Text)]}   -- [(link   fst, refer to as snd)]










showBzoFileModel :: Show a => BzoFileModel a -> String
showBzoFileModel (BzoFileModel mn pth dmn ast imp lnk impa lnka) =
  "\n \nModule: " ++ (show mn ) ++ "\nPath: " ++ (show pth) ++
    "\nDomain: "  ++ (show dmn) ++
    "\nImports: " ++ (show imp) ++ "\nAliased Imports: " ++ (show impa) ++
    "\nLinks: "   ++ (show lnk) ++ "\nAliased Links: "   ++ (show lnka) ++
    "\nAST:\n"    ++ (show ast)
instance (Show a) => Show (BzoFileModel a) where show = showBzoFileModel










replaceModel :: (Show a, Show b) => BzoFileModel a -> b -> BzoFileModel b
replaceModel (BzoFileModel mn fp dm _ fi fl ia la) x = (BzoFileModel mn fp dm x fi fl ia la)










adjustModel :: (Show a, Show b) => BzoFileModel a -> (a -> b) -> BzoFileModel b
adjustModel (BzoFileModel mn fp dm x fi fl ia la) f = (BzoFileModel mn fp dm (f x) fi fl ia la)









data Definition
 = FuncDef {
    identifier :: T.Text,
    hostfile   :: T.Text,
    typehead   :: TypeHeader,
    functype   :: Type,
    definitions:: [(T.Text, Expr)] }
 | TypeDef {
    identifier :: T.Text,
    hostfile   :: T.Text,
    typehead   :: TypeHeader,
    typedef    :: Type }
 | TyClassDef {
    identifier :: T.Text,
    hostfile   :: T.Text,
    typehead   :: TypeHeader,
    interface  :: [(T.Text, TypeHeader, Type)] }
 | FuncSyntax {
    identifier :: T.Text,
    hostfile   :: T.Text,
    ftyheader  :: BzoSyntax,
    funcsyntax :: [BzoSyntax] }
 | TypeSyntax {
    identifier :: T.Text,
    hostfile   :: T.Text,
    typesyntax :: BzoSyntax }
 | TyClassSyntax {
    identifier :: T.Text,
    hostfile   :: T.Text,
    typesyntax :: BzoSyntax }
  deriving Eq










showDefinition :: Definition -> String
showDefinition (FuncDef fnid file tyhd fty defs) = "  FNDEF:\n    " ++
                                              (show fnid)  ++ "\n    " ++
                                              (show file)  ++ "\n        " ++
                                              (show tyhd)  ++ "\n" ++
                                              (show fty)   ++ "\n" ++
                                              (show defs)  ++ "\n"

showDefinition (TypeDef tyid file thd defs) = "  TYDEF:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n        " ++
                                              (show thd)   ++ "\n" ++
                                              (show defs)  ++ "\n"
--
showDefinition (FuncSyntax fnid file hedr defs) = "  FNSYN:\n    " ++
                                              (show fnid)  ++ "\n    " ++
                                              (show file)  ++ "\n    T:  " ++
                                              (show hedr)  ++ "\n    D:  " ++
                                              (show defs)  ++ "\n"

showDefinition (TypeSyntax tyid file defs) = "  TYSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)
--
showDefinition (TyClassSyntax tyid file defs) = "  TCSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)
instance Show Definition where show = showDefinition











type FnId = Int64   -- Function Id
type TyId = Int64   -- Type Id
type VrId = Int64   -- Variable Id
type TVId = Int64   -- Type Variable Id
type LcId = Int64   -- Local Id










data ExprHeader = ExprHeader (M.Map LcId Atom) deriving (Eq, Show)

data Expr
  = CallExpr  BzoPos  FnId  [LcId] [LcId]
  | PhiExpr   BzoPos [FnId] [LcId] [LcId]
  | OpExpr    BzoPos Opcode [LcId] [LcId]
  | LetExpr   BzoPos ContextFrame  [Expr]
  | UnresExpr BzoPos BzoSyntax
  deriving (Show, Eq)










data Opcode =
  OP_ADD  | OP_SUB  | OP_MUL  | OP_DIV  | OP_MOD  |
  OP_XOR  | OP_OR   | OP_AND  | OP_NOT  | OP_PCT  |
  OP_SHL  | OP_SHR
  deriving (Show, Eq)










data Pattern
  = CmpdPtrn  BzoPos Type [Pattern]
  | FiltPtrn  BzoPos Type [(Pattern, Int64)]
  | PipePtrn  BzoPos Type [Pattern]
  | TVarPtrn  BzoPos Type TVId
  | VarPtrn   BzoPos Type VrId
  | IntPtrn   BzoPos Integer
  | FltPtrn   BzoPos Double
  | StrPtrn   BzoPos T.Text
  | WildPtrn  BzoPos
  | UnresPtrn BzoSyntax
  deriving (Show, Eq)










data TypeHeader = TyHeader (M.Map TVId Atom) deriving (Eq, Show)

data Type
  = UnresType BzoSyntax
  | ParamType BzoPos Pattern
  | FuncType  BzoPos Type Type
  | CmpdType  BzoPos [Type]
  | PolyType  BzoPos [Type]
  | MakeType  BzoPos [Type]
  | IntType   BzoPos Integer
  | FltType   BzoPos Double
  | StrType   BzoPos T.Text
  | VoidType  BzoPos
  | LtrlType  BzoPos TyId
  | TVarType  BzoPos TVId
  | ArryType  BzoPos Integer Type
  deriving (Show, Eq)










data Constraint = Constraint BzoPos Type  deriving (Eq, Show)










data Atom
  = IntAtom BzoPos Integer
  | FltAtom BzoPos Double
  | StrAtom BzoPos T.Text
  | FncAtom BzoPos T.Text T.Text FnId
  | TypAtom BzoPos T.Text T.Text TyId
  | VarAtom BzoPos T.Text [Constraint] Type
  | MutAtom BzoPos T.Text [Constraint] Type
  | TVrAtom BzoPos T.Text [Constraint] Type
  | ParAtom BzoPos T.Text [Constraint] Type
  | RetAtom BzoPos T.Text [Constraint] Type
  deriving (Show, Eq)

atomId :: Atom -> Maybe T.Text
atomId (FncAtom _ t _ _) = Just t
atomId (TypAtom _ t _ _) = Just t
atomId (VarAtom _ t _ _) = Just t
atomId (MutAtom _ t _ _) = Just t
atomId (TVrAtom _ t _ _) = Just t
atomId (ParAtom _ t _ _) = Just t
atomId (RetAtom _ t _ _) = Just t
atomId _ = Nothing

isIntAtom :: Atom -> Bool
isIntAtom (IntAtom _ _) = True
isIntAtom _             = False

isFltAtom :: Atom -> Bool
isFltAtom (FltAtom _ _) = True
isFltAtom _             = False

isStrAtom :: Atom -> Bool
isStrAtom (StrAtom _ _) = True
isStrAtom _             = False

isLitAtom :: Atom -> Bool
isLitAtom atm = (isIntAtom atm) || (isFltAtom atm) || (isStrAtom atm)

isFncAtom :: Atom -> Bool
isFncAtom (FncAtom _ _ _ _) = True
isFncAtom _                 = False

isTypAtom :: Atom -> Bool
isTypAtom (TypAtom _ _ _ _) = True
isTypAtom _                 = False

isVarAtom :: Atom -> Bool
isVarAtom (VarAtom _ _ _ _) = True
isVarAtom _                 = False

isMutAtom :: Atom -> Bool
isMutAtom (MutAtom _ _ _ _) = True
isMutAtom _                 = False

isTVrAtom :: Atom -> Bool
isTVrAtom (TVrAtom _ _ _ _) = True
isTVrAtom _                 = False

isParAtom :: Atom -> Bool
isParAtom (ParAtom _ _ _ _) = True
isParAtom _                 = False

isRetAtom :: Atom -> Bool
isRetAtom (RetAtom _ _ _ _) = True
isRetAtom _                 = False

isLocalAtom :: Atom -> Bool
isLocalAtom atm = (isVarAtom atm) || (isMutAtom atm)

isGlobalAtom :: Atom -> Bool
isGlobalAtom atm = (isFncAtom atm) || (isTypAtom atm)

isIOAtom :: Atom -> Bool
isIOAtom atm = (isParAtom atm) || (isRetAtom atm)










data DefinitionTable
  = DefinitionTable {
      dt_defs :: M.Map Int64 Definition,
      dt_files:: [BzoFileModel ([Int64], [Int64])], -- [(local defs, visible defs)]
      dt_ids  :: M.Map T.Text [Int64],
      dt_top  :: Int64 }
  deriving Show










data ContextFrame
  = ContextFrame{
      cf_atomMap :: M.Map Int64 Atom,
      cf_top     :: Int64,
      cf_index   :: Int64 }
  deriving (Show, Eq)

data Context = Context [ContextFrame]


addAtom :: Context -> Atom -> (Context, Int64)
addAtom (Context ((ContextFrame atoms top i):xs)) atom = (Context ((ContextFrame (M.insert (top+1) atom atoms) (top+1) (i+1)):xs), top+1)


addContext :: Context -> Context
addContext (Context ((ContextFrame atoms top i):xs)) = Context ((ContextFrame M.empty (top+1) (i+1)):(ContextFrame atoms top i):xs)
addContext (Context [])                              = Context [(ContextFrame M.empty 0 0)]


popContext :: Context -> Context
popContext (Context (_:xs)) = Context xs


getIxContext :: Context -> Int64 -> Maybe (Atom, Int64)
getIxContext (Context []) ix = Nothing
getIxContext (Context ((ContextFrame atoms top i):xs)) ix =
  if (ix > top)
    then Nothing
    else case (fmap (\x -> (x, i)) $ M.lookup ix atoms) of
          Nothing -> getIxContext (Context xs) ix
          ret     -> ret


findId :: Context -> T.Text -> Maybe (Int64, Int64)
findId (Context                              []) name = Nothing
findId (Context ((ContextFrame atoms top i):xs)) name =
  let pairs   = M.assocs atoms
      matches = L.filter (\(i, atm) -> name == (Mb.fromMaybe (T.pack "") (atomId atm))) pairs
  in  case matches of
        [] -> findId (Context xs) name
        ms -> Just (fst $ L.head ms, i)










data SymbolTable = SymbolTable DefinitionTable FilePath (M.Map Int64 T.Text)
