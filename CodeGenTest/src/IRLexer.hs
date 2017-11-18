module IRLexer where
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Data.Text










data IRPos = IRPos{
  ir_line   :: Int,
  ir_column :: Int,
  ir_fname  :: Text }
  deriving (Eq, Show)










data IRToken
  = FuncToken  IRPos Text
  | NodeToken  IRPos Int
  | TypeToken  IRPos Text
  | OpenBrace  IRPos
  | CloseBrace IRPos
  | DefFunc    IRPos
  | DefType    IRPos
  deriving (Eq, Show)










data IRErr = IRErr IRPos Text deriving Show










data IRLexerState = IRLexerState{
  lx_line   :: Int,
  lx_column :: Int,
  lx_offset :: Int,
  lx_fname  :: Text,
  lx_tokens :: [IRToken] }
  deriving (Eq, Show)










data IRLexer a = IRLexer { irlex :: String -> IRLexerState -> Either [IRErr] (a, IRLexerState) }










makeLexErr :: IRLexerState -> IRErr
makeLexErr (IRLexerState line column offset fname tokens) = (IRErr (IRPos line column fname) (pack "Lexer Error\n"))
