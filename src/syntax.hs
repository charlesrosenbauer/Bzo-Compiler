module BzoSyntax where
import BzoTypes










data BzoSyntax
    = BzS_FunDef {
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
        tyIn :: BzoSyntax,
        tyEx :: BzoSyntax }
    | BzS_Lambda {
        pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | BzS_Id {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_TyId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_MId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_BId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_BTId {
        pos  :: BzoPos,
        sid  :: String }
    | BzS_Int {
        pos  :: BzoPos,
        sint :: Integer }
    | BzS_Flt {
        pos  :: BzoPos,
        sflt :: Double }
    | BzS_Str {
        pos  :: BzoPos,
        sstr :: String }
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
    | BzS_ArrAtom {
        pos  :: BzoPos,
        atom :: BzoSyntax }
    | BzS_Block {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_Expr {
        pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_Modifiers {
        pos  :: BzoPos,
        mods :: [Modifier] }
    | BzS_Calls {
        pos   :: BzoPos,
        calls :: [BzoSyntax] }
    | BzS_Wildcard {
        pos   :: BzoPos }
    | BzS_Undefined {
        pos   :: BzoPos }
    deriving Eq










data Modifier
    = Arry
    | ArSz Integer
    | ArVr String
    | Mods [Modifier]
    | ModUnspecified
    deriving Eq










showMod  :: Modifier -> String
showMod (Arry)   = " [] "
showMod (ArSz i) = " [ " ++ (show i) ++ " ] "
showMod (ArVr v) = " [ " ++ v ++ " ] "
showMod (Mods m) = concatMap showMod m
instance Show Modifier where show = showMod










showAST :: BzoSyntax -> String
showAST (BzS_FunDef _ inpar fid expar def) = "  {FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "}\n\n"
showAST (BzS_TypDef _ par tid def)         = "  {TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "}\n\n"
showAST (BzS_FnTypeDef _ fid inpar expar)  = "  {FTDEF: " ++ (show fid) ++ " [ " ++ (show inpar) ++ " ] ;; [ " ++ (show expar) ++ " ]}\n\n"
showAST (BzS_Lambda _ par def)             = "  {LAMBDA: " ++ (show par) ++ " :: " ++ (show def) ++ "} "
showAST (BzS_Id _ i)                       = "  ID: " ++ (show i)
showAST (BzS_TyId _ i)                     = "  TYPE ID: " ++ (show i)
showAST (BzS_BId _ i)                      = "  BID: " ++ (show i)
showAST (BzS_BTId _ i)                     = "  BTID: " ++ (show i)
showAST (BzS_Int _ i)                      = "  INT: " ++ (show i)
showAST (BzS_Flt _ f)                      = "  FLT: " ++ (show f)
showAST (BzS_Str _ s)                      = "  STR: " ++ (show s)
showAST (BzS_Poly _ p)                     = "  {POLY: " ++ (concatMap showAST p) ++ "} "
showAST (BzS_Cmpd _ c)                     = "  {CMPD: " ++ (concatMap showAST c) ++ "} "
showAST (BzS_ArrAtom _ atm)                = "  ARRAY ATOM: " ++ (show atm) ++ " "
showAST (BzS_Block _ ex)                   = "  {BK: " ++ (concatMap showAST ex) ++ " }. "
showAST (BzS_Expr _ ex)                    = "  (EX: " ++ (concatMap showAST ex) ++ " ) "
showAST (BzS_Modifiers _ m)                = show m
showAST (BzS_Calls _ c)                    = concatMap (\s -> "CALL:: " ++ (show s) ++ "\n") c
showAST (BzS_Wildcard _)                   = "  _WILDCARD_ "
showAST (BzS_Undefined _)                  = "  UNDEFINED "
instance Show BzoSyntax where show = showAST
