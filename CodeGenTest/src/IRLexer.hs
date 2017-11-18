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










data IRToken
  = FuncToken  IRPos Text
  | NodeToken  IRPos Int
  | TypeToken  IRPos Text
  | OpenBrace  IRPos
  | CloseBrace IRPos
  | DefFunc    IRPos
  | DefType    IRPos










data IRLexerState = IRLexerState{
  lx_line   :: Int,
  lx_column :: Int,
  lx_offset :: Int,
  lx_fname  :: Text,
  lx_tokens :: [IRToken] }
