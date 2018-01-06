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
showErrPos p = "In file \"" ++ (fileName p) ++ "\", at " ++ (show $ line p) ++ ":" ++ (show $ column p) ++ " ::\n"










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
        fnid :: !String,
        def  :: !BzoSyntax }
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
        arrexprs :: ![BzoSyntax] }
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
  deriving (Eq, Show)









showAST :: BzoSyntax -> String
showAST (BzS_FunDef _ inpar fid expar def) = " {FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "} "
showAST (BzS_TypDef _ par tid def)         = " {TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "} "
showAST (BzS_FnTypeDef _ fid def)          = " {FTDEF: " ++ (show fid) ++ " [ " ++ (show def) ++ " ]} "
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
showAST (BzS_ArrayObj  _ o a)              = " <" ++ (show o) ++ " array: " ++ (Prelude.concatMap showAST a) ++ "> "
showAST (BzS_FilterObj _ o f)              = " <" ++ (show o) ++ " of type " ++ (show f) ++ "> "
showAST (BzS_CurryObj  _ o p)              = " <" ++ (show p) ++ " applied to " ++ (show o) ++ "> "
showAST (BzS_MapObj    _ o)                = " <" ++ (show o) ++ " .. > "
showAST (BzS_Token     _ t)                = (show t)
showAST (BzS_Undefined)                    = " UNDEFINED "
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










data TypeAST
      = TA_Cmpd {
          ta_pos :: !BzoPos,
          ta_exs :: ![TypeAST] }
      | TA_Poly {
          ta_pos :: !BzoPos,
          ta_exs :: ![TypeAST] }
      | TA_Expr {
          ta_pos :: !BzoPos,
          ta_exp :: !TypeAST,
          ta_nxt :: !TypeAST }
      | TA_Filt {
          ta_pos :: !BzoPos,
          ta_filt :: ![TypeAST],
          ta_exp :: !TypeAST }
      | TA_FnTy {
          ta_pos :: !BzoPos,
          ta_in  :: !TypeAST,
          ta_out :: !TypeAST }
      | TA_Enum {
          ta_pos :: !BzoPos,
          ta_id  :: !String,
          ta_supr:: !String,
          ta_exp :: !TypeAST }
      | TA_Record {
          ta_pos :: !BzoPos,
          ta_id  :: !String,
          ta_supr:: !String,
          ta_exp :: !TypeAST }
      | TA_Curry {
          ta_pos :: !BzoPos,
          ta_crs :: ![TypeAST],
          ta_exp :: !TypeAST }
      | TA_Arr {
          ta_pos :: !BzoPos,
          ta_szs :: ![Integer], -- | Size of Zero is General Array
          ta_exp :: !TypeAST }
      | TA_IntLit {
          ta_pos :: !BzoPos,
          ta_int :: !Integer }
      | TA_FltLit {
          ta_pos :: !BzoPos,
          ta_flt  :: !Double }
      | TA_StrLit {
          ta_pos :: !BzoPos,
          ta_str :: !String }
      | TA_FnLit  {
          ta_pos :: !BzoPos,
          ta_fn  :: !String }
      | TA_TyLit  {
          ta_pos :: !BzoPos,
          ta_ty  :: !String }
      | TA_ExFnLit  {
          ta_pos :: !BzoPos,
          ta_fn  :: !String,
          ta_loc :: !String }
      | TA_ExTyLit  {
          ta_pos :: !BzoPos,
          ta_ty  :: !String,
          ta_loc :: !String }
      | TA_BFnLit {
          ta_pos :: !BzoPos,
          ta_fn  :: !String }
      | TA_BTyLit {
          ta_pos :: !BzoPos,
          ta_ty  :: !String }
      | TA_TyVar {
          ta_pos :: !BzoPos,
          ta_id  :: !String }
      | TA_Nil{
          ta_pos :: !BzoPos }










data ModelRecord = ModelRecord{
    mr_pos    :: !BzoPos,
    mr_name   :: !String,
    mr_parent :: !String,
    mr_type   :: !TypeAST }










data ModelEnum = ModelEnum{
    me_pos    :: !BzoPos,
    me_name   :: !String,
    me_parent :: !String,
    me_type   :: !TypeAST }










data TParModel
  = TParModel {
      tp_pos  :: !BzoPos,
      tp_pars :: ![TParModel] }
  | TParVar   {
      tp_pos  :: !BzoPos,
      tp_id   :: !String,
      tp_filt :: ![TypeAST] }
  | TParNil










data FParModel
  = FParModel {
      fp_pos  :: !BzoPos,
      fp_pars :: ![FParModel] }
  | FParFilt {
      fp_pos  :: !BzoPos,
      fp_mod  :: !FParModel,
      fp_filt :: ![TypeAST] }
  | FParVar {
      fp_pos  :: !BzoPos,
      fp_id   :: !String }
  | FParTyp {
      fp_pos  :: !BzoPos,
      fp_id   :: !String }
  | FParInt {
      fp_pos  :: !BzoPos,
      fp_int  :: !Integer }
  | FParFlt {
      fp_pos  :: !BzoPos,
      fp_flt  :: !Double }
  | FParStr {
      fp_pos  :: !BzoPos,
      fp_str  :: !String }
  | FParWild {
      fp_pos  :: !BzoPos }
  | FParNilVal {
      fp_pos  :: !BzoPos }
  | FParNil










data ExprModel
  = EM_Block {
      em_pos :: !BzoPos,
      em_exs :: ![ExprModel] }
  | EM_Expr {
      em_pos :: !BzoPos,
      em_exp :: !ExprModel,
      em_nxt :: !ExprModel }
  | EM_Map {
      em_pos :: !BzoPos,
      em_exp :: !ExprModel }
  | EM_Filt {
      em_pos :: !BzoPos,
      em_exp :: !ExprModel,
      em_typ :: ![TypeAST] }
  | EM_Lambda {
      em_pos :: !BzoPos,
      em_par :: !FParModel,
      em_def :: !ExprModel }
  | EM_Curry {
      em_pos :: !BzoPos,
      em_ins :: ![ExprModel],
      em_exp :: !ExprModel }
  | EM_Cmpd {
      em_pos :: !BzoPos,
      em_xs  :: ![ExprModel] }
  | EM_Poly {
      em_pos :: !BzoPos,
      em_xs  :: ![ExprModel] }
  | EM_LitInt {
      em_pos :: !BzoPos,
      em_int :: !Integer }
  | EM_LitFlt {
      em_pos :: !BzoPos,
      em_flt :: !Double }
  | EM_LitStr {
      em_pos :: !BzoPos,
      em_str :: !String }
  | EM_MId {
      em_pos :: !BzoPos,
      em_id  :: !String }
  | EM_Id {
      em_pos :: !BzoPos,
      em_id  :: !String }
  | EM_TyId {
      em_pos :: !BzoPos,
      em_id  :: !String }
  | EM_BId {
      em_pos :: !BzoPos,
      em_id  :: !String }
  | EM_BTyId {
      em_pos :: !BzoPos,
      em_id  :: !String }
  | EM_Wildcard {
      em_pos :: !BzoPos }
  | EM_Nil {
      em_pos :: !BzoPos }
  | EM_ExFun {
      em_pos :: !BzoPos,
      em_id  :: !String,
      em_loc :: !String }
  | EM_ExTyp {
      em_pos :: !BzoPos,
      em_id  :: !String,
      em_loc :: !String }
  | EM_Hint {
      em_pos :: !BzoPos,
      em_hint:: !String,
      em_vars:: ![ExprModel] }









data CallAST
    = CA_TypeDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_pars    :: !TParModel,
        ca_records :: ![ModelRecord],
        ca_enums   :: ![ModelEnum],
        ca_tydef   :: !TypeAST }
    | CA_FTDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_intype  :: !TypeAST,
        ca_extype  :: !TypeAST }
    | CA_FnDefCall {
        ca_pos     :: !BzoPos,
        ca_id      :: !String,
        ca_in      :: !FParModel,
        ca_ex      :: !FParModel,
        ca_fndef   :: !ExprModel }
    | CA_HintCall {
        ca_pos     :: !BzoPos,
        ca_hint    :: !String,
        ca_hpars   :: ![ExprModel] }
    | CA_REPLCall {
        ca_pos     :: !BzoPos,
        ca_exdef   :: !ExprModel }
    | CA_Calls {
        ca_pos     :: !BzoPos,
        ca_calls   :: ![CallAST] }









showEnum :: ModelEnum -> String
showEnum (ModelEnum p n r t) = " {Enum : " ++ n ++ ", inside " ++ r ++ ", of type " ++ (show t) ++ " } "
instance Show ModelEnum where show = showEnum










showRecord :: ModelRecord -> String
showRecord (ModelRecord p n r t) = " {Record : " ++ n ++ ", inside " ++ r ++ ", of type " ++ (show t) ++ " } "
instance Show ModelRecord where show = showRecord










showCallAST :: CallAST -> String
showCallAST (CA_TypeDefCall      p i s r e t) = " {TyDefCall: " ++ i ++
                                            "\n   PARS: " ++ (show s) ++
                                            "\n   RECS: " ++ (Prelude.concatMap (\x -> (show x) ++ ", ") r) ++
                                            "\n   ENMS: " ++ (Prelude.concatMap (\x -> (show x) ++ ", ") e) ++
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










showTParModel :: TParModel -> String
showTParModel (TParModel p ps  ) = " ( " ++ (Prelude.concatMap show ps) ++ " ) "
showTParModel (TParVar   p x  f) = " { " ++ x ++ " : " ++ (show f) ++ " }, "
showTParModel (TParNil)          = " N/A "
instance Show TParModel where show = showTParModel










showFParModel :: FParModel -> String
showFParModel (FParModel  p ps  ) = " ( " ++ (Prelude.concatMap show ps) ++ " ) "
showFParModel (FParFilt   p x  f) = " { " ++ (show x) ++ " : " ++ (show f) ++ " }, "
showFParModel (FParVar    p x   ) = " { " ++ x ++ " }, "
showFParModel (FParTyp    p x   ) = " { " ++ x ++ " }, "
showFParModel (FParInt    p    i) = " { " ++ (show i) ++ " }, "
showFParModel (FParFlt    p    f) = " { " ++ (show f) ++ " }, "
showFParModel (FParStr    p    s) = " { " ++ s ++ " }, "
showFParModel (FParWild   p     ) = " { _ }, "
showFParModel (FParNilVal p     ) = " { () }, "
showFParModel (FParNil)          = " N/A "
instance Show FParModel where show = showFParModel










showTypeAST :: TypeAST -> String
showTypeAST (TA_Cmpd   p xs)     = " ( Cmpd:\n" ++ (Prelude.concatMap (\x -> "    " ++ (show x) ++ " .\n") xs) ++ ") "
showTypeAST (TA_Poly   p xs)     = " ( Poly:\n" ++ (Prelude.concatMap (\x -> "    " ++ (show x) ++ " ,\n") xs) ++ ") "
showTypeAST (TA_Expr   p x n)    = (show x) ++ " -> " ++ (show n)
showTypeAST (TA_Filt   p f x)    = " {" ++ (show x) ++ " ∪ " ++ (show f) ++ "} "
showTypeAST (TA_FnTy   p i o)    = " {" ++ (show i) ++ " ;; " ++ (show o) ++ "} "
showTypeAST (TA_Enum   p i s x)  = " { Enm: " ++ i ++ " ∪ " ++ (show x) ++ ", child of " ++ s ++ "} "
showTypeAST (TA_Record p i s x)  = " { Rcd: " ++ i ++ " ∪ " ++ (show x) ++ ", child of " ++ s ++ "} "
showTypeAST (TA_Curry  p cs x)   = " { Cur: " ++ (Prelude.concatMap (\y -> (show y) ++ " → ") cs) ++ " ⇒ " ++ (show x) ++ "} "
showTypeAST (TA_Arr    p ss x)   = " { Arr: " ++ (show x) ++ (Prelude.concatMap (\n -> ife (n /= 0) ("["++(show n)++"]") ("[?]")) ss) ++ "} "
showTypeAST (TA_IntLit p i)      = " <Int: " ++ (show i) ++ "> "
showTypeAST (TA_FltLit p f)      = " <Flt: " ++ (show f) ++ "> "
showTypeAST (TA_StrLit p s)      = " <Str: " ++ s ++ "> "
showTypeAST (TA_TyLit  p x)      = " <Ty: "  ++ x ++ "> "
showTypeAST (TA_FnLit  p x)      = " <Fn: "  ++ x ++ "> "
showTypeAST (TA_BFnLit p x)      = " <BFn: " ++ x ++ "> "
showTypeAST (TA_BTyLit p x)      = " <BTy: " ++ x ++ "> "
showTypeAST (TA_TyVar  p x)      = " <TVr: " ++ x ++ "> "
showTypeAST (TA_ExTyLit p x l)   = " <Ty: "  ++ x ++ ", from " ++ l ++ "> "
showTypeAST (TA_ExFnLit p x l)   = " <Fn: "  ++ x ++ ", from " ++ l ++ "> "
showTypeAST (TA_Nil    p)        = " <NIL ()> "
instance Show TypeAST where show = showTypeAST










showExprModel :: ExprModel -> String
showExprModel (EM_Block  _ xs   ) = " { Block:\n" ++ (Prelude.concatMap (\x -> "  " ++ (show x) ++ "\n") xs) ++ "} "
showExprModel (EM_Expr   _ ex nx) = (show ex) ++ " -> " ++ (show nx)
showExprModel (EM_Map    _ ex   ) = " <Map: " ++ (show ex) ++ " .. > "
showExprModel (EM_Lambda _ ps df) = " ( λ " ++ (show ps) ++ " . " ++  (show df) ++ ") " -- Add parameters later
showExprModel (EM_Filt   _ ex tp) = " ( Filt: " ++ (show ex) ++ " ∪ " ++ (show tp) ++ ") "
showExprModel (EM_Curry  _ is ex) = " ( " ++ (Prelude.concatMap (\x -> (show x) ++ " → ") is) ++ " ⇒ " ++ (show ex) ++ ") "
showExprModel (EM_Cmpd   _ xs   ) = " ( Cmpd:\n" ++ (Prelude.concatMap (\x -> "    " ++ (show x) ++ " .\n") xs) ++ ") "
showExprModel (EM_Poly   _ xs   ) = " ( Poly:\n" ++ (Prelude.concatMap (\x -> "    " ++ (show x) ++ " ,\n") xs) ++ ") "
showExprModel (EM_LitInt _ i    ) = " <Int: "  ++ (show i) ++ "> "
showExprModel (EM_LitFlt _ f    ) = " <Flt: "  ++ (show f) ++ "> "
showExprModel (EM_LitStr _ s    ) = " <Str: "  ++ s ++ "> "
showExprModel (EM_MId    _ i    ) = " <MId: "  ++ i ++ "> "
showExprModel (EM_Id     _ i    ) = " <Id: "   ++ i ++ "> "
showExprModel (EM_TyId   _ i    ) = " <Ty: "   ++ i ++ "> "
showExprModel (EM_BId    _ i    ) = " <BId: "  ++ i ++ "> "
showExprModel (EM_BTyId  _ i    ) = " <BTy: "  ++ i ++ "> "
showExprModel (EM_ExFun  _ i  l ) = " <ExFn: " ++ i ++ " from " ++ l ++ "> "
showExprModel (EM_ExTyp  _ i  l ) = " <ExTy: " ++ i ++ " from " ++ l ++ "> "
showExprModel (EM_Wildcard _    ) = " _ "
showExprModel (EM_Nil      _    ) = " () "
instance Show ExprModel where show = showExprModel










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










data Show a => Definition a
  = FnDefinition {
      def_ftdefs :: ![(TypeAST,   TypeAST  ,            BzoPos)],  -- InType, ExType, Pos
      def_fndefs :: ![(FParModel, FParModel, ExprModel, BzoPos)],  -- InPars, ExPars, Expr, Pos
      def_fnId   :: !a }   -- Id
  | TyDefinition {
      def_tydef  :: !(TParModel,  TypeAST, [ModelRecord], [ModelEnum], BzoPos),   -- TPars, Type, Records, Enums, Pos
      def_tyId   :: !a }   -- Id
  | RcDefinition {
      def_rcdef  :: !(TypeAST, a, BzoPos),    -- Type, Parent, Position
      def_rcId   :: !a }   -- Id
  | EnDefinition {
      def_endef  :: !(TypeAST, a, BzoPos),    -- Type, Parent, Position
      def_enId   :: !a }   -- Id
  | HintDefinition {
      def_hint   :: !([ExprModel], BzoPos),    -- Parameters, Position
      def_hintId :: !T.Text }  -- Hint
  | NilDefinition










showDefinition :: (Show a) => Definition a -> String
showDefinition (FnDefinition fts fns fnid) = "Function Defintion : " ++ (show fnid) ++ " { " ++
                                              "\n\n    FtDefs:\n" ++ (concatMap (\(a, b, _)    ->
                                                  "(" ++ (show a) ++ " ;; " ++ (show b) ++ "),\n") fts) ++
                                              "\n\n    FnDefs:\n" ++ (concatMap (\(a, b, c, d) ->
                                                  "(" ++ (show a) ++ " -> " ++ (show b) ++ " :: " ++ (show c) ++ "),\n") fns) ++ " }\n\n"

showDefinition (TyDefinition (tp, t, r, e, _) tyid) =
                                             "Type Defintion : " ++ (show tyid) ++ " { " ++
                                              "\n\n    TyDef:\n"   ++ (show tp) ++ " -> " ++ (show t) ++
                                              "\n\n    Records:\n" ++ (show r)  ++
                                              "\n\n    Enums:\n"   ++ (show e)  ++ " }\n\n"
showDefinition (RcDefinition (t, p, _) r)  = "Record Definition : " ++ (show r) ++ " { " ++
                                              "\n    Type   : " ++ (show t) ++ "\n" ++
                                              "\n    Parent : " ++ (show p) ++ " }\n\n"
showDefinition (EnDefinition (t, p, _) e)  = "Enum Definition : " ++ (show e) ++ " { " ++
                                              "\n    Type   : " ++ (show t) ++ "\n" ++
                                              "\n    Parent : " ++ (show p) ++ " }\n\n"
showDefinition (HintDefinition (ps, _) h)  = "Hint Definition : " ++ (show h) ++ " { " ++ (concatMap (\x -> (show x) ++ ", ") ps) ++ " }\n"
showDefinition (NilDefinition) = "NilDefinition\n\n"
instance (Show a) => Show (Definition a) where show = showDefinition










data BzoType
  = BT_Int  { bt_hash :: !BzoHash, bt_int :: !Integer }
  | BT_Flt  { bt_hash :: !BzoHash, bt_flt :: !Double }
  | BT_Str  { bt_hash :: !BzoHash, bt_str :: !T.Text }
  | BT_Nil  { bt_hash :: !BzoHash }
  | BT_Wild { bt_hash :: !BzoHash }
  | BT_TVar { bt_hash :: !BzoHash, bt_id  :: !Int64 }
  | BT_Type { bt_hash :: !BzoHash, bt_id  :: !Int64,   bt_file:: !Int64 }
  | BT_Func { bt_hash :: !BzoHash, bt_id  :: !Int64,   bt_file:: !Int64 }
  | BT_Cmpd { bt_hash :: !BzoHash, bt_typs:: ![BzoType] }
  | BT_Poly { bt_hash :: !BzoHash, bt_typs:: ![BzoType] }
  | BT_Expr { bt_hash :: !BzoHash, bt_typ :: !BzoType, bt_nxt :: !BzoType }
  | BT_Enum { bt_hash :: !BzoHash, bt_id  :: !Int64,   bt_typ :: !BzoType }
  | BT_FnTy { bt_hash :: !BzoHash, bt_inty:: !BzoType, bt_exty:: !BzoType }
  | BT_Filt { bt_hash :: !BzoHash, bt_typ :: !BzoType, bt_filt:: ![BzoType] }
  | BT_Arr  { bt_hash :: !BzoHash, bt_typ :: !BzoType, bt_size:: ![Int64] }
  | BT_BTyp { bt_hash :: !BzoHash, bt_id  :: !Int64 }
  | BT_BFun { bt_hash :: !BzoHash, bt_id  :: !Int64 }
  deriving Show










hashType :: BzoType -> Int64
hashType = bt_hash
instance Hashable BzoType where hash = hashType










data BzoPattern
  = BP_Int  !BzoHash !Integer
  | BP_Flt  !BzoHash !Double
  | BP_Str  !BzoHash !T.Text
  | BP_Nil  !BzoHash
  | BP_Wild !BzoHash
  | BP_Type !BzoHash !Int64
  | BP_Cmpd !BzoHash ![BzoPattern]
  | BP_Expr !BzoHash !BzoPattern !BzoPattern
  | BP_Filt !BzoHash !BzoType !BzoPattern
  | BP_Enum !BzoHash !Int64 !BzoPattern
  deriving Show










data TypeTable
  = TypeTable {
      tt_ftys :: !(M.Map Int64 [(BzoType, BzoPos)]),    -- | fn id#  -> [(type, position)]
      tt_ttys :: !(M.Map Int64  (BzoType, BzoPos) ) }   -- | ty id#  ->  (type, position)










data NameTable
  = NameTable {
      nt_fnids  :: !(M.Map T.Text [(Int64, Int64)]),   -- | fn id    -> [(id#s, file id#s)]
      nt_tyids  :: !(M.Map T.Text  (Int64, Int64) ),   -- | ty id    ->  (id#s, file id#s)
      nt_names  :: !(M.Map T.Text  [Int64]        ),   -- | @name    ->   file id#s
      nt_names' :: !(M.Map Int64    T.Text        )  } -- | file id# ->   @name
      deriving Show










-- Used to accelerate type/pattern comparison checks
type BzoHash = Int64










class (Eq a) => SetObj a where
  setsub  :: a -> a -> Bool
  setsup  :: a -> a -> Bool
  setelem :: a -> a -> Bool
  setfilt :: a -> a -> Bool
  (⊆) :: a -> a -> Bool
  a ⊆ b = setsub a b
  (⊇) :: a -> a -> Bool
  a ⊇ b = setsup a b
  (∈) :: a -> a -> Bool
  a ∈ b = setelem a b
  (∩) :: a -> a -> Bool
  a ∩ b = setfilt a b










data Show a => TypeObject a
  = LinearType{
      tobj_align :: !Int64,
      tobj_bytes :: !Int64,
      tobj_type  :: !a,
      tobj_name  :: !T.Text,
      tobj_id    :: !Int64,
      tobj_tvars ::  M.Map Int64 T.Text,
      tobj_deps  ::  S.Set Int64 }
  | RecursiveType{
      tobj_align :: !Int64,
      tobj_bytes :: !Int64,
      tobj_type  :: !a,
      tobj_name  :: !T.Text,
      tobj_id    :: !Int64,
      tobj_tvars ::  M.Map Int64 T.Text,
      tobj_deps  ::  S.Set Int64 }
  | IndirectRecType{
      tobj_align :: !Int64,
      tobj_bytes :: !Int64,
      tobj_type  :: !a,
      tobj_name  :: !T.Text,
      tobj_id    :: !Int64,
      tobj_tvars ::  M.Map Int64 T.Text,
      tobj_rdeps ::  S.Set Int64,
      tobj_deps  ::  S.Set Int64 }
  deriving Show
