module BzoTypes where
import Text.Parsec
import Text.ParserCombinators.Parsec










data BzoToken = TkStartTup  { spos :: SourcePos }
           | TkEndTup       { spos :: SourcePos }
           | TkStartDat     { spos :: SourcePos }
           | TkEndDat       { spos :: SourcePos }
           | TkStartDo      { spos :: SourcePos }
           | TkEndDo        { spos :: SourcePos }
           | TkSepExpr      { spos :: SourcePos }
           | TkSepPoly      { spos :: SourcePos }
           | TkFilterSym    { spos :: SourcePos }
           | TkLambdaSym    { spos :: SourcePos }
           | TkMutable      { spos :: SourcePos }
           | TkReference    { spos :: SourcePos }
           | TkWildcard     { spos :: SourcePos }
           | TkDefine       { spos :: SourcePos }
           | TkFnSym        { spos :: SourcePos }
           | TkTupEmpt      { spos :: SourcePos }
           | TkArrGnrl      { spos :: SourcePos }
           | TkArrMod       { spos :: SourcePos }
           | TkInt          { spos :: SourcePos, valInt :: Integer }
           | TkFlt          { spos :: SourcePos, valFlt :: Float   }
           | TkStr          { spos :: SourcePos, valStr :: String  }
           | TkId           { spos :: SourcePos, valId  :: String  }
           | TkTypeId       { spos :: SourcePos, valId  :: String  }
           | TkNewline --SourcePos
           | TkBuiltin      { spos :: SourcePos, valId  :: String  }
           deriving Eq










data BzoErr = Other
            | StringErr String
            | LexErr ParseError










-- Creating a custom position type so that Parsec types don't have to be moved around everywhere.
-- This also makes it easy to add extra information here later to pass around
data BzoPos = BzoPos {
    line     :: Int,
    column   :: Int,
    fileName :: String }
    deriving (Eq, Show)










getPos :: SourcePos -> BzoPos
getPos s = BzoPos (sourceLine s) (sourceColumn s) (sourceName s)










getTkPos :: BzoToken -> BzoPos
getTkPos t = getPos $ spos t
