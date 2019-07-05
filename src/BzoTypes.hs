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
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (SntxErr    p st) = "Syntax Error:\n " ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (DepErr       st) = "Dependency Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (CfgErr       st) = "Configuration Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (PrepErr    p st) = "Preprocessor Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (show $ fileName p) ++ "\", at line " ++ (show $ line p) ++ ", column " ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: !Int,
  column   :: !Int,
  fileName :: !T.Text }
  deriving Eq

compareBzoPos :: BzoPos -> BzoPos -> Ordering
compareBzoPos (BzoPos l0 c0 f0) (BzoPos l1 c1 f1)
  | (f0 == f1) && (l0 == l1) && (c0 == c1) = EQ
  | (f0 == f1) && (l0 == l1)               = compare c0 c1
  | (f0 == f1)                             = compare l0 l1
  | otherwise                              = compare f0 f1

instance Ord BzoPos where compare = compareBzoPos










showPos :: BzoPos -> String
showPos (BzoPos l c fname) = " [P:" ++ (show fname) ++ "@L:" ++ (show l) ++ ",C:" ++ (show c) ++ "] "
instance Show BzoPos where show = showPos











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
showAST (BzS_Cmpd _ c)                     = " {CMPD: " ++ (L.concat $ L.intersperse " , " $ L.reverse $ Prelude.map showAST c) ++ "} "
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
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    functype   :: !Type,
    definitions:: ![(BzoSyntax)] }
 | TypeDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    typedef    :: !Type }
 | TyClassDef {
    defpos     :: !BzoPos,
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typehead   :: !TypeHeader,
    interface  :: ![(T.Text, TypeHeader, Type)] }
 | FuncSyntax {
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    ftyheader  :: !BzoSyntax,
    funcsyntax :: ![BzoSyntax] }
 | TypeSyntax {
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typesyntax :: !BzoSyntax }
 | TyClassSyntax {
    identifier :: !T.Text,
    hostfile   :: !T.Text,
    typesyntax :: !BzoSyntax }
  deriving Eq


dtdef :: Definition -> Int64 -> Type
dtdef (FuncDef  _ _ _ _ t _) _ = t
dtdef (TypeDef  _ _ _ _ t  ) _ = t
dtdef (TyClassDef p _ _ _ i) c = TyCsType p c i










showDefinition :: Definition -> String
showDefinition (FuncDef _ fnid file tyhd fty defs) = "  FNDEF:\n    " ++
                                              "FNID: " ++ (show fnid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show tyhd)  ++ "\n    " ++
                                              "TYPE: " ++ (show fty)   ++ "\n    " ++
                                              "DEFS: " ++ (show defs)  ++ "\n\n"

showDefinition (TypeDef _ tyid file thd defs) = "  TYDEF:\n    " ++
                                              "TYID: " ++ (show tyid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show thd)   ++ "\n    " ++
                                              "DEFS: " ++ (show defs)  ++ "\n\n"

showDefinition (TyClassDef _ tcid file thd ifac) = "  TCDEF:\n    " ++
                                              "TCID: " ++ (show tcid)  ++ "\n    " ++
                                              "FILE: " ++ (show file)  ++ "\n    " ++
                                              "TYHD: " ++ (show thd)   ++ "\n    " ++
                                              "FNCS: " ++ (show ifac)  ++ "\n\n"

showDefinition (FuncSyntax fnid file hedr defs) = "  FNSYN:\n    " ++
                                              (show fnid)  ++ "\n    " ++
                                              (show file)  ++ "\n    T:  " ++
                                              (show hedr)  ++ "\n    D:  " ++
                                              (show defs)  ++ "\n"

showDefinition (TypeSyntax tyid file defs) = "  TYSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)

showDefinition (TyClassSyntax tyid file defs) = "  TCSYN:\n    " ++
                                              (show tyid)  ++ "\n    " ++
                                              (show file)  ++ "\n    D:  " ++
                                              (show defs)
instance Show Definition where show = showDefinition











type FnId = Int64   -- Function Id
type TyId = Int64   -- Type Id
type TCId = Int64   -- Type Class Id
type VrId = Int64   -- Variable Id
type TVId = Int64   -- Type Variable Id
type LcId = Int64   -- Local Id










data TypeHeader = TyHeader { header :: ![TVId], tvarmap :: !(M.Map TVId THeadAtom) } deriving (Eq, Show)

emptyheader :: TypeHeader
emptyheader = TyHeader [] M.empty

data Type
  = UnresType !BzoSyntax
  | ParamType !BzoPos !BzoSyntax
  | FuncType  !BzoPos !Type !Type
  | CmpdType  !BzoPos ![Type]
  | PolyType  !BzoPos ![Type]
  | MakeType  !BzoPos ![Type]
  | IntType   !BzoPos !Integer
  | FltType   !BzoPos !Double
  | StrType   !BzoPos !T.Text
  | VoidType  !BzoPos
  | LtrlType  !BzoPos !TyId
  | TVarType  !BzoPos !TVId
  | BITyType  !BzoPos !TyId
  | ArryType  !BzoPos !Integer !Type
  | FLitType  !BzoPos ![FnId]
  | TyCsType  !BzoPos !TCId    ![(T.Text, TypeHeader, Type)]
  | TCType    !BzoPos !TCId    -- Carrying around the interface is useful, but raises potential recursion problems.
  | InvalidType

eqType :: Type -> Type -> Bool
eqType (UnresType      _) (UnresType      _) = True
eqType (ParamType    _ a) (ParamType    _ b) = (a == b)
eqType (FuncType _ i0 o0) (FuncType _ i1 o1) = (i0 == i1) && (o0 == o1)
eqType (CmpdType _    xs) (CmpdType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (PolyType _    xs) (PolyType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (MakeType _    xs) (PolyType _    ys) = ((L.length xs) == (L.length ys)) && (L.all (\(a,b) -> a == b) $ L.zip xs ys)
eqType (IntType  _     a) (IntType  _     b) = (a == b)
eqType (FltType  _     a) (FltType  _     b) = (a == b)
eqType (StrType  _     a) (StrType  _     b) = (a == b)
eqType (VoidType _      ) (VoidType _      ) = True
eqType (LtrlType _    t0) (LtrlType _    t1) = (t0 == t1)
eqType (TVarType _    v0) (TVarType _    v1) = (v0 == v1)
eqType (BITyType _    b0) (BITyType _    b1) = (b0 == b1)
eqType (ArryType _ i0 t0) (ArryType _ i1 t1) = (i0 == i1) && (t0 == t1)
eqType (FLitType _    f0) (FLitType _    f1) = ((L.sort f0) == (L.sort f1))
eqType (TyCsType _ i0  _) (TyCsType _ i1  _) = (i0 == i1)
eqType (TCType   _    i0) (TCType   _    i1) = (i0 == i1)
eqType InvalidType        InvalidType        = True
eqType _ _ = False
instance Eq Type where (==) = eqType
                       (/=) = (\a b-> not $ eqType a b)

typos :: Type -> BzoPos
typos (UnresType   ast) = pos ast
typos (ParamType p _  ) = p
typos (FuncType  p _ _) = p
typos (CmpdType  p   _) = p
typos (PolyType  p   _) = p
typos (MakeType  p   _) = p
typos (IntType   p   _) = p
typos (FltType   p   _) = p
typos (StrType   p   _) = p
typos (VoidType  p    ) = p
typos (LtrlType  p   _) = p
typos (TVarType  p   _) = p
typos (BITyType  p   _) = p
typos (ArryType  p _ _) = p
typos (FLitType  p   _) = p
typos (TyCsType  p _ _) = p
typos (TCType    p   _) = p










showType :: Type -> String
showType (UnresType     ast) = "[UnresType " ++ (show ast) ++ "]"
showType (ParamType _   pat) = "[ParamType " ++ (show pat) ++ "]"
showType (FuncType  _ ta tb) = "[FuncType "  ++ (show  ta) ++ " " ++ (show tb) ++ "]"
showType (CmpdType  _    ts) = "[CmpdType "  ++ (L.concat $ L.intersperse " , "  $ L.reverse $ L.map show ts) ++ "]"
showType (PolyType  _    ts) = "[PolyType "  ++ (L.concat $ L.intersperse " , "  $ L.reverse $ L.map show ts) ++ "]"
showType (MakeType  _    ts) = "[MakeType "  ++ (L.concat $ L.intersperse " >> " $ L.reverse $ L.map show ts) ++ "]"
showType (IntType   _     i) = "[IntLit "    ++ (show   i) ++ "]"
showType (FltType   _     f) = "[FltLit "    ++ (show   f) ++ "]"
showType (StrType   _     s) = "[StrLit "    ++ (show   s) ++ "]"
showType (VoidType  _      ) = "[VoidType]"
showType (LtrlType  _    ty) = "TY:" ++ (show ty)
showType (TVarType  _    tv) = "TV:" ++ (show tv)
showType (BITyType  _    bt) = "BT:" ++ (show bt)
showType (ArryType  _  i ty) = "[ArrayType [" ++ (show i) ++ "] " ++ (show ty) ++ "]"
showType (FLitType  _    fn) = "FN:" ++ (show fn)
showType (TyCsType  _  tc _) = "[TC:" ++ (show tc) ++ "]"
showType (TCType    _    tc) = "TC:" ++ (show tc)
showType (InvalidType      ) = "INVALIDTYPE"
instance Show Type where show = showType










data Constraint = Constraint !BzoPos !Type  deriving Eq

showConstraint :: Constraint -> String
showConstraint (Constraint p t) = "[Constraint: " ++ (show t) ++ "]"
instance Show Constraint where show = showConstraint










data THeadAtom
  = TVrAtom !BzoPos !T.Text ![Constraint]
  deriving Eq

showTHead :: THeadAtom -> String
showTHead (TVrAtom _ k cs) = "[" ++ (show k) ++ " . " ++ (show cs) ++ "]"
instance Show THeadAtom where show = showTHead

atomId :: THeadAtom -> T.Text
atomId (TVrAtom _ t _) = t










data DefinitionTable
  = DefinitionTable {
      dt_defs :: !(M.Map Int64 Definition),
      dt_files:: ![BzoFileModel ([Int64], [Int64])], -- [(local defs, visible defs)]
      dt_ids  :: !(M.Map T.Text [Int64]),
      dt_top  :: !Int64 }
  deriving Show
