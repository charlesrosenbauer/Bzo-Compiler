module AST where
import Data.Int
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Mb
import Tokens
import Error
import HigherOrder










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
    | BzS_ImplDef {
        pos    :: !BzoPos,
        imid   :: !T.Text,
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
    | BzS_ImplHead {
        pos     :: !BzoPos,
        imid    :: !T.Text,
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
showAST (BzS_ImplDef    _ imp tid def)     = " {IMPLDEF: " ++ (show imp) ++ " implements " ++ (show tid) ++ " w/ " ++ (concatMap (\x -> "\n\n  " ++ show x) def) ++ "}\n"
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
showAST (BzS_ImplHead _ _ _ _)             = "IMPLHEAD "
showAST (BzS_LispHead _ _ _)               = "LSPHEAD "
showAST (BzS_ArrHead  _ _)                 = "ARRHEAD "
showAST _                                  = " <???> "
instance Show BzoSyntax where show = showAST
