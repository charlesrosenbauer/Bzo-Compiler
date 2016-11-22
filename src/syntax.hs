module BzoSyntax where










data BzoSyntax
    = FunDef
    | TypDef
    | Atoms {vals :: [Atom] }
    | Tuple [BzoSyntax]
    | Poly  [BzoSyntax]
    | Statements [[BzoSyntax]]
    deriving (Eq, Show)









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    deriving (Eq, Show)
