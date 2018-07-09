module BzoTypes where
import Data.Int
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
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
  | TkStr          { spos :: !BzoPos, valStr :: !String  }
  | TkId           { spos :: !BzoPos, valId  :: !String  }
  | TkTypeId       { spos :: !BzoPos, valId  :: !String  }
  | TkMutId        { spos :: !BzoPos, valId  :: !String  }
  | TkTyVar        { spos :: !BzoPos, valId  :: !String  }
  | TkNewline      { spos :: !BzoPos }
  | TkBuiltin      { spos :: !BzoPos, valId  :: !String  }
  | TkBIType       { spos :: !BzoPos, valId  :: !String  }
  | TkNil
  deriving Eq










data BzoErr = Other
  | StringErr { position::BzoPos, errorStr::String }
  | LexErr    { position::BzoPos, errorStr::String }
  | ParseErr  { position::BzoPos, errorStr::String }
  | TypeErr   { position::BzoPos, errorStr::String }
  | SntxErr   { position::BzoPos, errorStr::String }
  | DepErr    { errorStr::String }
  | ParamErr  { errorStr::String }
  | CfgErr    { errorStr::String }
  | ModelErr  { position::BzoPos, errorStr::String }
  | PrepErr   { position::BzoPos, errorStr::String}










showBzErr :: BzoErr -> String
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ st
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ st
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ st
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ st
showBzErr (SntxErr    p st) = "Syntax Error:\n " ++ (showErrPos p) ++ st
showBzErr (DepErr       st) = "Dependency Error:\n" ++ st
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ st
showBzErr (CfgErr       st) = "Configuration Error:\n" ++ st
showBzErr (PrepErr    p st) = "Preprocessor Error:\n" ++ (showErrPos p) ++ st
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (fileName p) ++ "\", at line " ++ (show $ line p) ++ ", column " ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: !Int,
  column   :: !Int,
  fileName :: !String }
  deriving (Eq, Show)










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
    = BzS_Int {
        pos  :: !BzoPos,
        sint :: !Integer }
    | BzS_Flt {
        pos  :: !BzoPos,
        sflt :: !Double }
    | BzS_Str {
        pos  :: !BzoPos,
        sstr :: !String }
    | BzS_Id {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_TyId {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_TyVar {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_Curry {
        pos  :: !BzoPos,
        obj  :: !BzoSyntax }
    | BzS_MId {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_BId {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_BTId {
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_Namespace{
        pos  :: !BzoPos,
        sid  :: !String }
    | BzS_MapMod {
        pos  :: !BzoPos }
    | BzS_Wildcard {
        pos   :: !BzoPos }
    | BzS_Filter {
        pos   :: !BzoPos,
        filt  :: !BzoSyntax }
    | BzS_ArrGenMod{
        pos   :: !BzoPos }
    | BzS_ArrSzMod{
        pos   :: !BzoPos,
        sint  :: !Integer }
    | BzS_ArrExprMod{
        pos   :: !BzoPos,
        def   :: !BzoSyntax }
    | BzS_Nil {
        pos   :: !BzoPos }
    | BzS_Lambda {
        pos  :: !BzoPos,
        pars :: !BzoSyntax,
        def  :: !BzoSyntax }
    | BzS_Expr {
        pos   :: !BzoPos,
        exprs :: ![BzoSyntax] }
    | BzS_Box {
        pos   :: !BzoPos,
        expr  :: !BzoSyntax }
    | BzS_Poly {
        pos   :: !BzoPos,
        exprs :: ![BzoSyntax] }
    | BzS_Cmpd {
        pos   :: !BzoPos,
        exprs :: ![BzoSyntax] }
    | BzS_FnTy {
        pos   :: !BzoPos,
        tyIn  :: !BzoSyntax,
        tyEx  :: !BzoSyntax }
    | BzS_Block {
        pos   :: !BzoPos,
        exprs :: ![BzoSyntax] }
    | BzS_FunDef {
        pos    :: !BzoPos,
        inpars :: !BzoSyntax,
        fnid   :: !String,
        expars :: !BzoSyntax,
        def    :: !BzoSyntax }
    | BzS_TypDef {
        pos  :: !BzoPos,
        pars :: !BzoSyntax,
        tyid :: !String,
        typ  :: !BzoSyntax }
    | BzS_FnTypeDef {
        pos  :: !BzoPos,
        pars :: !BzoSyntax,
        fnid :: !String,
        def  :: !BzoSyntax }
    | BzS_TyClassDef {
        pos  :: !BzoPos,
        pars :: !BzoSyntax,
        tyid :: !String,
        defs :: ![BzoSyntax] }
    | BzS_Calls {
        pos   :: !BzoPos,
        calls :: ![BzoSyntax] }
    | BzS_ExTypObj {
        pos       :: !BzoPos,
        sid       :: !String,
        namespace :: !String }
    | BzS_ExFunObj {
        pos       :: !BzoPos,
        sid       :: !String,
        namespace :: !String }
    | BzS_ArrayObj {
        pos      :: !BzoPos,
        bzobj    :: !BzoSyntax,
        arrszs   :: ![Integer] }
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
    | BzS_ArrSzObj {
        pos     :: !BzoPos,
        sint    :: !Integer }
    | BzS_ArrGnObj {
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
    | BzS_Statement {
        pos     :: !BzoPos,
        expr    :: !BzoSyntax }
    | BzS_FnHead {
        pos     :: !BzoPos,
        inpars  :: !BzoSyntax,
        fnid    :: !String,
        expars  :: !BzoSyntax }
    | BzS_TyHead {
        pos     :: !BzoPos,
        pars    :: !BzoSyntax,
        tyid    :: !String }
    | BzS_TyClassHead {
        pos     :: !BzoPos,
        pars    :: !BzoSyntax,
        tyid    :: !String,
        defs    :: ![BzoSyntax] }
    | BzS_Import {
        pos     :: !BzoPos,
        fname   :: !String,
        frename :: !String }
    | BzS_Include {
        pos     :: !BzoPos,
        fname   :: !String,
        frename :: !String }
    | BzS_TyHint {
        pos     :: !BzoPos,
        inpars  :: !BzoSyntax,
        htid    :: !String,
        expars  :: !BzoSyntax }
    | BzS_FnHint {
        pos     :: !BzoPos,
        inpars  :: !BzoSyntax,
        htid    :: !String,
        expars  :: !BzoSyntax }
    | BzS_File {
        pos     :: !BzoPos,
        mname   :: !String,
        fname   :: !String,
        includes:: ![BzoSyntax],
        imports :: ![BzoSyntax],
        defs    :: ![BzoSyntax] }
    | BzS_Undefined
    deriving Eq










data CfgSyntax
  = LibLine {
      cpos    :: BzoPos,
      libName :: String,
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
showAST (BzS_Poly _ p)                     = " {POLY: " ++ (Prelude.concatMap showAST p) ++ "} "
showAST (BzS_Cmpd _ c)                     = " {CMPD: " ++ (Prelude.concatMap showAST c) ++ "} "
showAST (BzS_Block _ ex)                   = " {BK: " ++ (Prelude.concatMap showAST ex) ++ " } "
showAST (BzS_Expr _ ex)                    = " (EX: " ++ (Prelude.concatMap showAST ex) ++ " ) "
showAST (BzS_Statement _ ex)               = " (STMT: " ++ (showAST ex) ++ " ) "
showAST (BzS_Box  _ ex)                    = " (BX: " ++ (showAST ex) ++ ") "
showAST (BzS_Calls _ c)                    = Prelude.concatMap (\s -> " CALL:: " ++ (show s) ++ "\n") c
showAST (BzS_Wildcard _)                   = " _ "
showAST (BzS_MapMod _)                     = " .. "
showAST (BzS_ArrGenMod _)                  = " [] "
showAST (BzS_ArrSzMod _ i)                 = " [ " ++ (show  i) ++ " ] "
showAST (BzS_ArrGnObj _)                   = " [] "
showAST (BzS_ArrSzObj _ i)                 = " [ " ++ (show  i) ++ " ] "
showAST (BzS_ArrExprMod _ ex)              = " [ " ++ (show ex) ++ " ] "
showAST (BzS_Nil _)                        = " () "
showAST (BzS_TyVar _ i)                    = " TyVr: " ++ (show i)
showAST (BzS_ExTypObj _ o n)               = " <" ++ (show o) ++ " from " ++ (show n) ++ "> "
showAST (BzS_ExFunObj _ o n)               = " <" ++ (show o) ++ " from " ++ (show n) ++ "> "
showAST (BzS_ArrayObj  _ o a)              = " <" ++ (show o) ++ " array: " ++ (Prelude.concatMap (\x -> "[" ++ show x ++ "], ") a) ++ "> "
showAST (BzS_FilterObj _ o f)              = " <" ++ (show o) ++ " of type " ++ (show f) ++ "> "
showAST (BzS_CurryObj  _ o p)              = " <" ++ (show p) ++ " applied to " ++ (show o) ++ "> "
showAST (BzS_MapObj    _ o)                = " <" ++ (show o) ++ " .. > "
showAST (BzS_Token     _ t)                = (show t)
showAST (BzS_TyHint    _ ins h exs)        = " {Ty HINT: " ++ (show ins) ++ " -> " ++ h ++ " -> " ++ (show exs) ++ "} \n"
showAST (BzS_FnHint    _ ins h exs)        = " {Fn HINT: " ++ (show ins) ++ " -> " ++ h ++ " -> " ++ (show exs) ++ "} \n"

showAST (BzS_Import    _ fn fr)            = "    -- Import  " ++ fn ++ " as " ++ fr ++ " --\n"
showAST (BzS_Include   _ fn fr)            = "    -- Include " ++ fn ++ " as " ++ fr ++ " --\n"
showAST (BzS_File      _ mn fn ins ims dfs)= "\n-- (File, Module): (" ++ fn ++ ", " ++ mn ++ ")\nIncludes:\n" ++ (concatMap show ins) ++ "\nImports:\n" ++ (concatMap show ims) ++ "\nDefs:\n" ++ (concatMap show dfs)

showAST (BzS_Undefined)                    = " UNDEFINED "
showAST _                                  = " <???> "
instance Show BzoSyntax where show = showAST










data (Show a) => BzoFileModel a
  = BzoFileModel{
      bfm_moduleName    :: !String,
      bfm_filepath      :: !FilePath,
      bfm_domain        :: !String,
      bfm_fileModel     :: !a,
      bfm_fileImports   :: ![String],
      bfm_fileLinks     :: ![String],
      bfm_fileImportsAs :: ![(String, String)],   -- [(import fst, refer to as snd)]
      bfm_fileLinksAs   :: ![(String, String)]}   -- [(link   fst, refer to as snd)]










showBzoFileModel :: Show a => BzoFileModel a -> String
showBzoFileModel (BzoFileModel mn pth dmn ast imp lnk impa lnka) =
  "\n \nModule: " ++ mn ++ "\nPath: " ++ pth ++
    "\nDomain: "  ++ (show dmn) ++
    "\nImports: " ++ (show imp) ++ "\nAliased Imports: " ++ (show impa) ++
    "\nLinks: "   ++ (show lnk) ++ "\nAliased Links: "   ++ (show lnka) ++
    "\nAST:\n"    ++ (show ast)
instance (Show a) => Show (BzoFileModel a) where show = showBzoFileModel










data CallAST
    = CA_TypeDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_pars    :: !Expr,
        ca_tydef   :: !Expr }
    | CA_FTDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_intype  :: !Expr,
        ca_extype  :: !Expr }
    | CA_FnDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_in      :: !Expr,
        ca_ex      :: !Expr,
        ca_fndef   :: !Expr }
    | CA_HintCall {
        ca_pos     :: !BzoPos,
        ca_hint    :: !String,
        ca_hpars   :: ![Expr] }
    | CA_REPLCall {
        ca_pos     :: !BzoPos,
        ca_exdef   :: !Expr }
    | CA_Calls {
        ca_pos     :: !BzoPos,
        ca_calls   :: ![CallAST] }










showCallAST :: CallAST -> String
showCallAST (CA_TypeDefCall          p i s t) = " {TyDefCall: " ++ i ++
                                            "\n   PARS: " ++ (show s) ++
                                            "\n   DEF : " ++ (show t) ++ " }\n"
showCallAST (CA_FTDefCall            p x i o) = " {FnTyDefCall: " ++ x ++
                                            "\n   INPUT : " ++ (show i) ++
                                            "\n   OUTPUT: " ++ (show o) ++ " }\n"
showCallAST (CA_FnDefCall          p f i e d) = " {FnDefCall: " ++ f ++
                                            "\n   IN  PARS : " ++ (show i) ++
                                            "\n   OUT PARS : " ++ (show e) ++
                                            "\n   DEF      : " ++ (show d) ++ " }\n"
showCallAST (CA_HintCall             p h xs) = " {HintCall: " ++ h ++ "\n" ++
                                            "\n   PARS     : " ++ (show xs) ++ " }\n"
showCallAST (CA_REPLCall             p   xs) = " {REPL CALL:\n" ++ (show xs) ++ "\n}\n"
showCallAST (CA_Calls                p   xs) = " Modelled Calls:\n" ++ (Prelude.concatMap (\x -> (show x) ++ "\n") xs) ++ "\n"
instance Show CallAST where show = showCallAST










data Atom
  = Atm_Id  [Int64]
  | Atm_Ty  [Int64]
  | Atm_Mut  Int64
  | Atm_TVr  Int64
  | Atm_BIF  Int64
  | Atm_BIT  Int64
  | Atm_Nil
  | Atm_Wild










showAtom :: Atom -> String
showAtom (Atm_Id  ids) = " <Atm_Id  " ++ (show ids) ++ "> "
showAtom (Atm_Ty  ids) = " <Atm_Ty  " ++ (show ids) ++ "> "
showAtom (Atm_Mut idx) = " <Atm_Mut " ++ (show idx) ++ "> "
showAtom (Atm_TVr idx) = " <Atm_TVr " ++ (show idx) ++ "> "
showAtom (Atm_BIF idx) = " <Atm_BIF " ++ (show idx) ++ "> "
showAtom (Atm_BIT idx) = " <Atm_BIT " ++ (show idx) ++ "> "
showAtom (Atm_Nil    ) = " <Atm_Nil > "
showAtom (Atm_Wild   ) = " <Atm_Wild > "
instance Show Atom where show = showAtom










data Decor
  = Dcr_Curry  (Atom, [Atom ])
  | Dcr_Array  (Atom, [Int64])
  | Dcr_Map     Atom
  | Dcr_Filter (Atom, Expr)










showDecor :: Decor -> String
showDecor (Dcr_Curry  (atm, atms)) = " <Dcr_Curry " ++ (show atm) ++ " <- { " ++ (show atms) ++ " }> "
showDecor (Dcr_Array  (atm, szs )) = " <Dcr_Array " ++ (show atm) ++ " of size [ " ++ (show szs) ++ " ]> "
showDecor (Dcr_Map     atm       ) = " <Dcr_Map   " ++ (show atm) ++ " }> "
showDecor (Dcr_Filter (atm, expr)) = " <Dcr_Filt  " ++ (show atm) ++ " : " ++ (show expr) ++ " > "
instance Show Decor where show = showDecor










data Expr
  = Exp_Dec   Decor
  | Exp_Atm   Atom
  | Exp_Cmpd  [Expr]
  | Exp_Poly  [Expr]
  | Exp_Block [Expr]










showExpr :: Expr -> String
showExpr (Exp_Dec   dec) = " <Expr_Dec   "  ++ (show dec) ++  "> "
showExpr (Exp_Atm   atm) = " <Expr_Atm   "  ++ (show atm) ++  "> "
showExpr (Exp_Cmpd  xps) = " <Expr_Cmpd  (" ++ (show xps) ++ ")> "
showExpr (Exp_Poly  xps) = " <Expr_Poly  (" ++ (show xps) ++ ")> "
showExpr (Exp_Block xps) = " <Expr_Block {" ++ (show xps) ++ "}> "
instance Show Expr where show = showExpr









data SymbolTable
  = SymbolTable {
      st_iids   :: !(M.Map T.Text [(Int64, Int64)]),   -- To look up if an Identifier exists, and if so, what are the Table Indices(snd) and Files(fst)?
      st_fids   :: !(M.Map T.Text Int64),              -- To look up if a File Identifier exists, and if so, what is the Table Index?
      st_itable :: !(M.Map Int64 (T.Text, Int64)),     -- For a given Table Index, what is the associated Identifier, and the file where it's defined?
      st_ftable :: !(M.Map Int64 T.Text),              -- For a given Table Index, what is the associated File Identifier?
      st_dmids  :: !(M.Map T.Text [Int64]),            -- For a given File Domain, what are the associated File Indices?
      st_itop   :: !Int64,                           -- What is the highest used Identifier Table Index?
      st_ftop   :: !Int64 }                          -- What is the highest used File Table Index?
  deriving Show
