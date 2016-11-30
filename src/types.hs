module BzoTypes where
import Text.Parsec
import Text.ParserCombinators.Parsec










data BzoToken = TkStartTup  { pos :: SourcePos }
           | TkEndTup       { pos :: SourcePos }
           | TkStartDat     { pos :: SourcePos }
           | TkEndDat       { pos :: SourcePos }
           | TkStartDo      { pos :: SourcePos }
           | TkEndDo        { pos :: SourcePos }
           | TkSepExpr      { pos :: SourcePos }
           | TkSepPoly      { pos :: SourcePos }
           | TkFilterSym    { pos :: SourcePos }
           | TkLambdaSym    { pos :: SourcePos }
           | TkMutable      { pos :: SourcePos }
           | TkReference    { pos :: SourcePos }
           | TkWildcard     { pos :: SourcePos }
           | TkDefine       { pos :: SourcePos }
           | TkFnSym        { pos :: SourcePos }
           | TkTupEmpt      { pos :: SourcePos }
           | TkArrGnrl      { pos :: SourcePos }
           | TkArrMod       { pos :: SourcePos }
           | TkInt          { pos :: SourcePos, valInt :: Integer }
           | TkFlt          { pos :: SourcePos, valFlt :: Float   }
           | TkStr          { pos :: SourcePos, valStr :: String  }
           | TkId           { pos :: SourcePos, valId  :: String  }
           | TkTypeId       { pos :: SourcePos, valId  :: String  }
           | TkNewline --SourcePos
           | TkBuiltin      { pos :: SourcePos, valId  :: String  }
           deriving Eq










data BzoErr = Other
            | StringErr String
            | LexErr ParseError
