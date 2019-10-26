{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2019 Charles Rosenbauer

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.-}

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
