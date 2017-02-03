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
        typ  :: DtType }
    | BzS_FnTypeDef {
        pos  :: BzoPos,
        fnid :: String,
        tyIn :: DtType,
        tyEx :: DtType }
    | BzS_Lambda {
        pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | BzS_Atoms {
        pos  :: BzoPos,
        atom :: Atom }
    | BzS_ArrAtom {
        pos  :: BzoPos,
        atom :: Atom }
    | BzS_Type {
        pos  :: BzoPos,
        typ  :: DtType }
    | BzS_Statements {
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









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    | AtmMut String
    | AtmBI  String
    | AtmTBI String
    deriving Eq










data DtType
    = DtNilType
    | DtTuple       { dtyps :: [DtType] }
    | DtCoreType    { id    :: String }
    | DtTypeVar     { id    :: String }
    | DtBIType      { id    :: String }
    | DtFunc        { dtyIn :: DtType, dtyEx :: DtType }
    | DtPolymorph   { dtyps :: [DtType] }
    | DtModded      { dmod  :: Modifier, dtyp :: DtType }
    | DtRecord      { recs :: [(String, DtType)] }
    | DtFilter      { dtyp  :: DtType, dfilter :: DtType }
    | DtUnspecified
    deriving (Eq, Show)










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










showAtom :: Atom -> String
showAtom (AtmInt i) = show i
showAtom (AtmFlt f) = show f
showAtom (AtmStr s) = show s
showAtom (AtmId  i) = show i
showAtom (AtmMut m) = show m
showAtom (AtmBI  s) = show s
showAtom (AtmTBI t) = show t
instance Show Atom where show = showAtom










showAST :: BzoSyntax -> String
showAST (BzS_FunDef _ inpar fid expar def) = "{FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "}\n\n"
showAST (BzS_TypDef _ par tid def)         = "{TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "}\n\n"
showAST (BzS_FnTypeDef _ fid inpar expar)  = "{FTDEF: " ++ (show fid) ++ " [ " ++ (show inpar) ++ " ] ;; [ " ++ (show expar) ++ " ]}\n\n"
showAST (BzS_Lambda _ par def)             = " {LAMBDA: " ++ (show par) ++ " :: " ++ (show def) ++ "} "
showAST (BzS_Atoms _ atm)                  = " ATOM: " ++ (show atm) ++ " "
showAST (BzS_ArrAtom _ atm)                = " ARRAY ATOM: " ++ (show atm) ++ " "
showAST (BzS_Type _ _)                     = " TYPE "
showAST (BzS_Statements _ ex)              = " {ST: " ++ (concatMap showAST ex) ++ " }. "
showAST (BzS_Expr _ ex)                    = " (EX: " ++ (concatMap showAST ex) ++ " ) "
showAST (BzS_Modifiers _ m)                = show m
showAST (BzS_Calls _ c)                    = concatMap (\s -> "CALL:: " ++ (show s) ++ "\n") c
showAST (BzS_Wildcard _)                   = " _WILDCARD_ "
showAST (BzS_Undefined _)                  = " UNDEFINED "
instance Show BzoSyntax where show = showAST
