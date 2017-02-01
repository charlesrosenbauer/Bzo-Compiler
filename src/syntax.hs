module BzoSyntax where
import BzoTypes










data BzoSyntax
    = BzS_FunDef {
        pos     :: BzoPos,
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
        fnid   :: String,
        tyIn   :: DtType,
        tyEx   :: DtType }
    | BzS_Lambda {
        pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | BzS_Atoms {
        pos  :: BzoPos,
        atom  :: Atom }
    | BzS_ArrAtom {
        pos  :: BzoPos,
        atom  :: Atom }
    | BzS_Type {
        pos :: BzoPos,
        mods :: [Modifier],
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
        calls :: [BzoSyntax] }
    | BzS_TupleExpr {
        pos :: BzoPos,
        exprs :: [BzoSyntax] }
    | BzS_StatementBlock {
        pos :: BzoPos,
        inpar :: BzoSyntax,
        expar :: BzoSyntax,
        exprs :: [BzoSyntax] }
    | BzS_DataType {
        pos :: BzoPos,
        typ :: DtType }
    | BzS_Construct {
        pos :: BzoPos,
        tyId  :: String,
        modf  :: Modifier }
    | BzS_Wildcard
    | BzS_Undefined
    deriving Eq









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    | AtmBI  String
    | AtmTBI String
    deriving Eq










data RecUnit = [(String, DtType)]
  deriving (Eq, Show)










data DtType
    = DtNilType
    | DtTuple       { dtyps :: [DtType] }
    | DtCoreType    { id    :: String }
    | DtTypeVar     { id    :: String }
    | DtBIType      { id    :: String }
    | DtFunc        { dtyIn :: DtType, dtyEx :: DtType }
    | DtPolymorph   { dtyps :: [DtType] }
    | DtModded      { dmod  :: Modifier, dtyp :: DtType }
    | DtRecord      { recs  :: [RecUnit] }
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










makeMods :: BzoSyntax -> Modifier
makeMods (Modifiers x) = Mods $ x
makeMods _ = Mods []










showMod  :: Modifier -> String
showMod (Mutb)   = " ~ "
showMod (Refr)   = " @ "
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
showAtom (AtmBI  s) = show s
instance Show Atom where show = showAtom










showAST :: BzoSyntax -> String
showAST (FunDef inpar fid expar def) = "{FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "}\n\n"
showAST (TypDef par tid def)         = "{TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "}\n\n"
showAST (FnTypeDef fid inpar expar)  = "{FTDEF: " ++ (show fid) ++ " [ " ++ (show inpar) ++ " ] ;; [ " ++ (show expar) ++ " ]}\n\n"
showAST (Lambda par def)             = " {LAMBDA: " ++ (show par) ++ " :: " ++ (show def) ++ "} "
showAST (Atoms atm)                  = " ATOM: " ++ (show atm) ++ " "
showAST (ArrAtom atm)                = " ARRAY ATOM: " ++ (show atm) ++ " "
showAST (Type _ _)                   = " TYPE "
showAST (Statements ex)              = " {ST: " ++ (concatMap showAST ex) ++ " }. "
showAST (Expr ex)                    = " (EX: " ++ (concatMap showAST ex) ++ " ) "
showAST (TupleExpr ex)               = " (TU: " ++ (concatMap showAST ex) ++ " ) "
showAST (Modifiers m)                = show m
showAST (Calls c)                    = concatMap (\s -> "CALL:: " ++ (show s) ++ "\n") c
showAST (Wildcard)                   = " _WILDCARD_ "
showAST (Undefined)                  = " UNDEFINED "
showAST (StatementBlock i x def)     = "{BLOCK from " ++ (show i) ++ " to " ++ (show x) ++ ": " ++ (concatMap show def) ++ " }"
showAST (Construct   i _)            = "(CONS: " ++ (show i) ++ ")"
showAST (DataType d)                 = " TYPE: " ++ (show d) ++ " "
instance Show BzoSyntax where show = showAST
