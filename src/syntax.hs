module BzoSyntax where
import BzoTypes










data BzoSyntax
    = FunDef {
        --pos     :: BzoPos,
        inpars :: BzoSyntax,
        fnid   :: String,
        expars :: BzoSyntax,
        def    :: BzoSyntax }
    | TypDef {
        --pos  :: BzoPos,
        pars :: BzoSyntax,
        tyid :: String,
        typ  :: DtType }
    | FnTypeDef {
        --pos  :: BzoPos,
        fnid   :: String,
        tyIn   :: DtType,
        tyEx   :: DtType }
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
    | DataType {
        --pos :: BzoPos,
        typ :: DtType }
    | Construct {
        --pos :: BzoPos,
        tyId  :: String,
        modf  :: Modifier }
    | Wildcard
    | Undefined
    deriving Eq









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    | AtmBI  String
    deriving Eq










data RecUnit = RecUnit{ rid :: String, rtyp :: DtType }
  deriving (Eq, Show)










data DtType
    = DtNilType
    | DtTuple       { dtyps :: [DtType] }
    | DtCoreType    { id    :: String }
    | DtTypeVar     { id    :: String }
    | DtBIType      { id    :: String }
    | DtFunc        { dtyIn :: DtType, dtyEx :: DtType }
    | DtPolymorph   { typs  :: [DtType] }
    | DtModded      { dmod  :: Modifier, dtyp :: DtType }
    | DtRecord      { recs  :: [RecUnit] }
    | DtUnspecified
    deriving (Eq, Show)










data Modifier
    = Mutb
    | Refr
    | Arry
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
showAST (Modifiers m)                = show m
showAST (Calls c)                    = concatMap (\s -> "CALL:: " ++ (show s) ++ "\n") c
showAST (Wildcard)                   = " _WILDCARD_ "
showAST (Undefined)                  = " UNDEFINED "
showAST (StatementBlock i x def)     = "{BLOCK from " ++ (show i) ++ " to " ++ (show x) ++ ": " ++ (concatMap show def) ++ " }"
showAST (Construct   i _)            = "(CONS: " ++ (show i) ++ ")"
showAST _                            = "\n!!UNKNOWN!!\n"
instance Show BzoSyntax where show = showAST
