module DefinitionTable where
import BzoTypes
import Data.Int
import HigherOrder
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import qualified Data.Maybe      as Mb
import qualified Data.List       as L










addDef :: DefinitionTable -> Definition -> DefinitionTable
addDef (DefinitionTable defs fs ids top) def =
  let defs' = M.insert (top+1) def defs
      defid = identifier def
      ids'  = if (M.member defid ids)
                then M.adjust (L.insert (top+1)) defid ids
                else M.insert defid [top+1] ids
  in (DefinitionTable defs' fs ids' (top+1))










queryDef :: DefinitionTable -> Int64 -> Maybe Definition
queryDef (DefinitionTable defs fs ids top) defid = M.lookup defid defs










queryIds :: DefinitionTable -> T.Text -> [Int64]
queryIds (DefinitionTable defs fs ids top) defid =
  case M.lookup defid ids of
    Just xs -> xs
    Nothing -> []










queryIdFile :: DefinitionTable -> T.Text -> T.Text -> [Int64]
queryIdFile dtab@(DefinitionTable defs fs ids top) defid fileid =
  let defids = queryIds dtab defid
  in L.filter (matchHostfile dtab fileid) defids
  where matchHostfile :: DefinitionTable -> T.Text -> Int64 -> Bool
        matchHostfile dtab fileid defid =
          case (queryDef dtab defid) of
            Just df -> (identifier df) == fileid
            Nothing -> False
