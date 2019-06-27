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










checkTyClass :: DefinitionTable -> (TypeHeader, Type, TCId) -> [BzoErr]
checkTyClass (DefinitionTable defs files ids _) (thead, ty, tc) =
  let
      -- Get typeclass definition and associated interface
      tclass :: Definition
      tclass = defs M.! tc

      intfc  :: [(Text, TypeHeader, Type)]
      intfc = interface tclass


      -- Get local visibility
      visibility :: [Int64]
      visibility = snd $ bfm_fileModel $ L.head $
                   L.filter (\x -> (pack $ bfm_filepath x) == (fileName $ typos ty)) $ files

      -- Line up interfaces


      -- Filter out functions that do not match interface

      fns :: [[(Text, Int64)]]
      fns = []

  in if (L.null fns) || (L.any L.null fns)
      then [TypeErr (typos ty) $ pack $ "Type " ++ (show ty) ++ " does not match class " ++ (show tc) ++ "\n"]
      else []
