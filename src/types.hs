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
           | TkArrMod SourcePos
           | TkInt SourcePos Integer
           | TkFlt SourcePos Float
           | TkStr SourcePos String
           | TkId SourcePos String
           | TkTypeId SourcePos String
           | TkNewline --SourcePos
           | TkBuiltin SourcePos String
           deriving Eq










data BzoErr = Other
            | StringErr String
            | LexErr ParseError
