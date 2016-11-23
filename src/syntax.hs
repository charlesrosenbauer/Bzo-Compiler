module BzoSyntax where










data BzoSyntax
    = FunDef {
        inpars :: BzoSyntax,
        fnid :: Atom,
        outpars :: BzoSyntax,
        def :: BzoSyntax }
    | TypDef
    | Atoms {vals :: [Atom] }
    | Tuple [BzoSyntax]
    | Poly  [BzoSyntax]
    | Statements {exprs :: [BzoSyntax]}
    | Expr {exprs :: [BzoSyntax]}
    | Undefined
    deriving (Eq, Show)









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    deriving (Eq, Show)
