module BzoTypes where
import Text.Parsec
import Text.ParserCombinators.Parsec










data BzoToken = TkStartTup SourcePos
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
           | TkNewline --SourcePos
           | TkBuiltin SourcePos String
           | TkDefFn SourcePos [BzoToken]
           | TkDefTy SourcePos [BzoToken]
           | TkDefFnTy SourcePos [BzoToken]
           | TkDefTyCt SourcePos [BzoToken]
           | TkDefVr SourcePos [BzoToken]
           deriving Eq










data DtType = NilType
            | Tuple [DtType]
            | CoreType String
            | TypeVar String
            | DtFunc FnType
            | DtPolymorph [DtType]
            | DtUnspecified
            deriving Eq










data FnType = Func DtType DtType
            | FnPolymorph [FnType]
            | FnUnspecified
            deriving Eq










data BzoErr = Other
            | StringErr String
            | LexErr ParseError
            







