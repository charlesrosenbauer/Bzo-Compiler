module TypeClass where
import BzoTypes
import TypeChecker
import Query
import HigherOrder
import Data.Text
import Data.Int
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe as Mb
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
makeTyClassObj   :: DefinitionTable -> Definition -> TyClassObj
makeTyClassObj dt@(DefinitionTable defs files ids top) (TyClassDef tcid file thd itf) =
  let
      grabInterface :: (Text, TypeHeader, Type) -> [Int64]
      grabInterface (tid, thead, ty) =
        let
            tidpass :: [Int64]
            tidpass = Mb.fromMaybe [] $ M.lookup tid ids

            -- Filter : typecheck
            chkpass :: [Int64]
            chkpass =
              let
                  getDef :: Int64 -> (Int64, Definition)
                  getDef x = (x, defs M.! x)

                  checkMatch :: (Int64, Definition) -> Bool
                  checkMatch (_,x) = L.null $ checkType dt (thead, ty) (typehead x, functype x)

              in  L.map fst $
                  L.filter checkMatch $
                  L.map getDef tidpass

        in chkpass

      {-
        TODO:
          * figure out how to determine which type a function is an interface to
          * assemble interfaces from types that fully match
      -}

  in  TyClassObj M.empty










makeTyClassTable :: DefinitionTable -> TyClassTable
makeTyClassTable (DefinitionTable defs files ids top) =
  let
      classes :: [(Int64, Definition)]
      classes = L.filter (\(a,b) -> isTyClass b) $ assocs defs



  in TyClassTable M.empty
