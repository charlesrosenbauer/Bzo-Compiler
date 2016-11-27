module BzoSyntax where










data BzoSyntax
    = FunDef {
        inpars  :: BzoSyntax,
        fnid    :: Atom,
        outpars :: BzoSyntax,
        def     :: BzoSyntax }
    | TypDef {
        pars :: BzoSyntax,
        typ  :: DtType }
    | Lambda {
        pars :: BzoSyntax,
        def  :: BzoSyntax }
    | Atoms Atom
    | Type DtType
    | Statements {exprs :: [BzoSyntax] }
    | Expr {exprs :: [BzoSyntax] }
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
