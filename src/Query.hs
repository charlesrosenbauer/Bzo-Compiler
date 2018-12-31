module Query where
import BzoTypes
import Data.Map.Strict as M
import Data.Set.Strict as S
import Data.Text










data Environment = Environment DefinitionTable SymbolTable Context Text
