module TypeClass where
import BzoTypes
import HigherOrder
import Data.Text
import Data.Map.Strict
import Debug.Trace




data TyClassObj = TyClassObj Map TyId [(Text, FnId)]

data TyClassTable = TyClassTable Map TCId TyClassObj
