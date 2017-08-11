module BzoSyntax where
import BzoTypes










data BzoSyntax
    = BzS_Int {
        pos  :: BzoPos,
        sint :: Integer }
    | BzS_Flt {
        pos  :: BzoPos,
        sflt :: Double }
    | BzS_Str {
        pos  :: BzoPos,
        sstr :: String }
    | BzS_Id {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_TyId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_TyVar {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_Curry {
        pos  :: BzoPos,
        obj  :: BzoSyntax }
    | BzS_MId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_BId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_BTId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_Namespace{
        pos  :: BzoPos,
        sid  :: String }
    | BzS_MapMod {
        pos  :: BzoPos }
    | BzS_Wildcard {
        pos   :: BzoPos }
    | BzS_Filter {
        pos   :: BzoPos,
        filt  :: BzoSyntax }
    | BzS_ArrGenMod{
        pos   :: BzoPos }
    | BzS_ArrSzMod{
        pos   :: BzoPos,
        sint  :: Integer }
    | BzS_ArrExprMod{
        pos   :: BzoPos,
        def   :: BzoSyntax }
    | BzS_Nil {
        pos   :: BzoPos }
    | BzS_Lambda {
        pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | BzS_Expr {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_Box {
        pos   :: BzoPos,
        expr  :: BzoSyntax }
    | BzS_Poly {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_Cmpd {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_FnTy {
        pos   :: BzoPos,
        tyIn  :: BzoSyntax,
        tyEx  :: BzoSyntax }
    | BzS_Block {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_FunDef {
        pos    :: BzoPos,
        inpars :: BzoSyntax,
        fnid   :: String,
        expars :: BzoSyntax,
        def    :: BzoSyntax }
    | BzS_TypDef {
        pos  :: BzoPos,
        pars :: BzoSyntax,
        tyid :: String,
        typ  :: BzoSyntax }
    | BzS_FnTypeDef {
        pos  :: BzoPos,
        fnid :: String,
        def  :: BzoSyntax }
    | BzS_Calls {
        pos   :: BzoPos,
        calls :: [BzoSyntax] }
    | BzS_ExTypObj {
        pos       :: BzoPos,
        sid       :: String,
        namespace :: String }
    | BzS_ExFunObj {
        pos       :: BzoPos,
        sid       :: String,
        namespace :: String }
    | BzS_ArrayObj {
        pos      :: BzoPos,
        bzobj    :: BzoSyntax,
        arrexprs :: [BzoSyntax] }
    | BzS_FilterObj {
        pos     :: BzoPos,
        bzobj   :: BzoSyntax,
        filt     :: BzoSyntax }
    | BzS_CurryObj {
        pos     :: BzoPos,
        bzobj   :: BzoSyntax,
        crypars :: [BzoSyntax] }
    | BzS_MapObj {
        pos     :: BzoPos,
        bzobj   :: BzoSyntax }
    | BzS_ArrSzObj {
        pos     :: BzoPos,
        sint    :: Integer }
    | BzS_ArrGnObj {
        pos     :: BzoPos }
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
showAST (BzS_Poly _ p)                     = " {POLY: " ++ (concatMap showAST p) ++ "} "
showAST (BzS_Cmpd _ c)                     = " {CMPD: " ++ (concatMap showAST c) ++ "} "
showAST (BzS_Block _ ex)                   = " {BK: " ++ (concatMap showAST ex) ++ " } "
showAST (BzS_Expr _ ex)                    = " (EX: " ++ (concatMap showAST ex) ++ " ) "
showAST (BzS_Box  _ ex)                    = " (BX: " ++ (showAST ex) ++ ") "
showAST (BzS_Calls _ c)                    = concatMap (\s -> " CALL:: " ++ (show s) ++ "\n") c
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
showAST (BzS_ArrayObj  _ o a)              = " <" ++ (show o) ++ " array: " ++ (concatMap showAST a) ++ "> "
showAST (BzS_FilterObj _ o f)              = " <" ++ (show o) ++ " of type " ++ (show f) ++ "> "
showAST (BzS_CurryObj  _ o p)              = " <" ++ (show p) ++ " applied to " ++ (show o) ++ "> "
showAST (BzS_MapObj    _ o)                = " <" ++ (show o) ++ " .. > "
showAST (BzS_Undefined)                     = " UNDEFINED "
instance Show BzoSyntax where show = showAST
