module BzoSyntax where
import BzoTypes










data BzoSyntax
    = FunDef {
        --pos     :: BzoPos,
        inpars :: BzoSyntax,
        fnid   :: Atom,
        expars :: BzoSyntax,
        def    :: BzoSyntax }
    | TypDef {
        --pos  :: BzoPos,
        pars :: BzoSyntax,
        tyid :: Atom,
        typ  :: DtType }
    | Lambda {
        --pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | Atoms {
        --pos  :: BzoPos,
        atom  :: Atom }
    | ArrAtom {
        --pos  :: BzoPos,
        atom  :: Atom }
    | Type {
        --pos :: BzoPos,
        mods :: [Modifier],
        typ  :: DtType }
    | Statements {
        --pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | Expr {
        --pos   :: BzoPos,
        exprs :: [BzoSyntax] }
    | Modifiers {
        --pos  :: BzoPos,
        mods :: [Modifier] }
    | Calls {
        calls :: [BzoSyntax] }
    | TupleExpr {
        --pos :: BzoPos,
        exprs :: [BzoSyntax] }
    | StatementBlock {
        --pos :: BzoPos,
        inpar :: BzoSyntax,
        expar :: BzoSyntax,
        exprs :: [BzoSyntax] }
    | Constructor {
        --pos :: BzoPos,
        tid :: String,
        typ :: DtType }
    | DataType {
        --pos :: BzoPos,
        typ :: DtType }
    | Wildcard
    | Undefined
    deriving Eq









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    deriving Eq










data DtType
    = NilType Modifier
    | Tuple Modifier [DtType]
    | CoreType Modifier String
    | TypeVar Modifier String
    | DtFunc Modifier FnType
    | DtPolymorph Modifier [DtType]
    | DtUnspecified Modifier
    deriving (Eq, Show)










data FnType
    = Func DtType DtType
    | FnPolymorph [FnType]
    | FnUnspecified
    deriving (Eq, Show)










data Modifier
    = Mutb
    | Refr
    | Arry
    | ArSz Integer
    | Mods [Modifier]
    | ModUnspecified
    deriving Eq










showMod  :: Modifier -> String
showMod (Mutb)   = " ~ "
showMod (Refr)   = " @ "
showMod (Arry)   = " [] "
showMod (ArSz i) = " [ " ++ (show i) ++ " ] "
showMod (Mods m) = concatMap showMod m
instance Show Modifier where show = showMod










showAtom :: Atom -> String
showAtom (AtmInt i) = show i
showAtom (AtmFlt f) = show f
showAtom (AtmStr s) = show s
showAtom (AtmId  i) = show i
instance Show Atom where show = showAtom










showAST :: BzoSyntax -> String
showAST (FunDef inpar fid expar def) = "{FNDEF: " ++ (show inpar) ++ " -> " ++ (show fid) ++ " -> " ++ (show expar) ++ " :: " ++ (show def) ++ "}\n\n"
showAST (TypDef par tid def)         = "{TYDEF: " ++ (show tid) ++ " [ " ++ (show par) ++ " ] :: " ++ (show def) ++ "}\n\n"
showAST (Lambda par def)             = " {LAMBDA: " ++ (show par) ++ " :: " ++ (show def) ++ "} "
showAST (Atoms atm)                  = " ATOM: " ++ (show atm) ++ " "
showAST (ArrAtom atm)                = " ARRAY ATOM: " ++ (show atm) ++ " "
showAST (Type _ _)                   = " TYPE "
showAST (Statements ex)              = " {ST: " ++ (concatMap showAST ex) ++ " }. "
showAST (Expr ex)                    = " (EX: " ++ (concatMap showAST ex) ++ " ) "
showAST (TupleExpr ex)               = " (TU: " ++ (concatMap showAST ex) ++ " ) "
showAST (Modifiers m)                = concatMap show m
showAST (Calls c)                    = concatMap (\s -> "CALL:: " ++ (show s) ++ "\n") c
showAST (Wildcard)                   = " _WILDCARD_ "
showAST (Undefined)                  = " UNDEFINED "
showAST (StatementBlock i x def)     = "{BLOCK from " ++ (show i) ++ " to " ++ (show x) ++ ": " ++ (concatMap show def) ++ " }"
showAST (Constructor i _)            = "(CONS: " ++ (show i) ++ ")"
showAST _                            = "\n!!UNKNOWN!!\n"
instance Show BzoSyntax where show = showAST
