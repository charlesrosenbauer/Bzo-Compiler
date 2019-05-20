module TypeClass where
import BzoTypes
import HigherOrder
import Data.Text
import Data.Map.Strict
import Debug.Trace




data TyClassObj = TyClassObj Map TyId [(Text, FnId)]

data TyClassTable = TyClassTable Map TCId TyClassObj










{-
  TODO:
    Add Typeclass Checking function
      - probably generate a table for typeclass lookups
      - each typeclass has an interface
      - table should contain a mapping from types with valid interfaces to
          their interfaces
-}
