module BzoTypes where










data Token = TkStartTup
           | TkEndTup
           | TkStartDat
           | TkEndDat
           | TkStartDo
           | TkEndDo
           | TkSepExpr
           | TkSepPoly
           | TkFilterSym
           | TkLambdaSym
           | TkMutable
           | TkReference
           | TkWildcard
           | TkDefine
           | TkFnSym
           | TkTupEmpt
           | TkArrGnrl
           | TkExpGnrl
           | TkArrMod
           | TkInt Integer
           | TkFlt Float
           | TkStr String
           | TkId String
           | TkTypeId String
           | TkVariable String
           | TkFunction String
           | TkTypeVar String
           | TkLambda
           | TkExpr
           | TkNewline
           | TkBuiltin String










data DtType = NilType
            | Tuple [DtType]
            | CoreType String
            | TypeVar String
            | DtFunc FnType
            | DtPolymorph [DtType]










data FnType = Func DtType DtType
            | FnPolymorph [FnType]



