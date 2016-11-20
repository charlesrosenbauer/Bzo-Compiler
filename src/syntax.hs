module BzoSyntax where










data BzoProgram
    = FunDef
    | TypDef
    | Atoms {vals :: [Atom] }
    deriving (Eq, Show)









data Atom
    = AtmInt Integer
    | AtmFlt Float
    | AtmStr String
    | AtmId  String
    deriving (Eq, Show)



