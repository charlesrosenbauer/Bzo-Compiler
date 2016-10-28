module BzoTypes where
import Text.Parsec
import Text.ParserCombinators.Parsec










data Token = TkStartTup SourcePos
           | TkEndTup SourcePos
           | TkStartDat SourcePos
           | TkEndDat SourcePos
           | TkStartDo SourcePos
           | TkEndDo SourcePos
           | TkSepExpr SourcePos
           | TkSepPoly SourcePos
           | TkFilterSym SourcePos
           | TkLambdaSym SourcePos
           | TkMutable SourcePos
           | TkReference SourcePos
           | TkWildcard SourcePos
           | TkDefine SourcePos
           | TkFnSym SourcePos
           | TkTupEmpt SourcePos
           | TkArrGnrl SourcePos
           | TkExpGnrl SourcePos
           | TkArrMod SourcePos
           | TkInt SourcePos Integer
           | TkFlt SourcePos Float
           | TkStr SourcePos String
           | TkId SourcePos String
           | TkTypeId SourcePos String
           | TkVariable SourcePos String DtType
           | TkFunction SourcePos String FnType
           | TkTypeVar SourcePos String
           | TkLambda SourcePos
           | TkExpr SourcePos
           | TkNewline SourcePos
           | TkBuiltin SourcePos String
           | TkDefFn SourcePos [Token]
           | TkDefTy SourcePos [Token]
           | TkDefFnTy SourcePos [Token]
           | TkDefTyCt SourcePos [Token]
           | TkDefVr SourcePos [Token]










data DtType = NilType
            | Tuple [DtType]
            | CoreType String
            | TypeVar String
            | DtFunc FnType
            | DtPolymorph [DtType]
            | DtUnspecified










data FnType = Func DtType DtType
            | FnPolymorph [FnType]
            | FnUnspecified



