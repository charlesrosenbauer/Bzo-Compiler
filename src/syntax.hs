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
        typ  :: DtType }
    | Lambda {
        --pos  :: BzoPos,
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | Atoms {
        --pos  :: BzoPos,
        atom  :: Atom }
    | ArrAtoms {
        --pos  :: BzoPos,
        atom  :: Atom }
    | Type {
        --pos :: BzoPos,
        typ :: DtType }
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
    | Wildcard
    | Undefined
    deriving (Eq, Show)









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    deriving (Eq, Show)










data DtType
    = NilType
    | Tuple [DtType]
    | CoreType String
    | TypeVar String
    | DtFunc FnType
    | DtPolymorph [DtType]
    | DtUnspecified
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
    deriving (Eq, Show)
