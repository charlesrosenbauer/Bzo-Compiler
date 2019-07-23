module TypeMapGen where
import BzoEmulatorTypes
import Data.Map.Strict as M
import Data.Set as S
import Data.Int
import Core
import BzoTypes










mapStruct :: DefinitionTable -> TyId -> TypeMap
mapStruct dt tid = Struct tid


mapUnion  :: DefinitionTable -> TyId -> TypeMap
mapUnion dt tid =
  let

  in Union tid (S.empty)


mapClass  :: DefinitionTable -> TyId -> TypeMap
mapClass dt tid =
  let

  in Class tid (M.empty)
