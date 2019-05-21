module TypeClass where
import BzoTypes
import Query
import HigherOrder
import Data.Text
import Data.Int
import Data.List as L
import Data.Map.Strict as M
import Debug.Trace




data TyClassObj = TyClassObj (Map TyId [(Text, FnId)])

data TyClassTable = TyClassTable (Map TCId TyClassObj)










{-
  TODO:
    Add Typeclass Checking function
      - probably generate a table for typeclass lookups
      - each typeclass has an interface
      - table should contain a mapping from types with valid interfaces to
          their interfaces
-}
makeTyClassTable :: DefinitionTable -> TyClassTable
makeTyClassTable (DefinitionTable defs files ids top) =
  let
      classes :: [(Int64, Definition)]
      classes = L.filter (\(a,b) -> isTyClass b) $ assocs defs

  in TyClassTable M.empty
