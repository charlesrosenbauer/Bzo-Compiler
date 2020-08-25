{-
This is the bootstrapping compiler for the Bzo programming language.
Copyright (C) 2020 Charles Rosenbauer

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

module BzoFileSorter where
import BzoTypes
import BzoParser
import BzoPreprocessor
import GHC.Exts
import Data.Either
import Data.Maybe
import Data.Text as T
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import HigherOrder
import ModelRules
import Core
import Error
import Debug.Trace










getImportDependencies :: Show a => BzoFileModel a -> [Text]
getImportDependencies fs = nub $ (\(BzoFileModel mn fp dm ast im ln ia la) -> im ++ (L.map fst ia)) fs










getLinkDependencies :: Show a => [BzoFileModel a] -> [Text]
getLinkDependencies fs = nub $ L.concatMap (\(BzoFileModel mn fp dm ast im ln ia la) -> ln ++ (L.map fst la)) fs










orderByImports :: Show a => Map Text (BzoFileModel a) -> [BzoFileModel a] -> [BzoFileModel a] -> Either [BzoErr] [BzoFileModel a]
orderByImports mp out [] = Right out
orderByImports mp out fs =
  let (remain, next) = L.break (\x -> containsManyMembers mp $ getImportDependencies x) fs
      next'          = L.map (\x -> (bfm_moduleName x, x)) next
      domain         = if (bfm_domain (L.head fs) == (pack "@"))
                        then pack "Project Files"
                        else bfm_domain $ L.head fs
  in case next of
    [] -> Left [CfgErr ((pack "Unsatisfiable Dependencies in ") `T.append` domain `T.append` (pack "! Compilation cannot continue.\n"))]
    nx -> orderByImports (insertMany mp next') (out ++ next) remain










orderByLinks :: Show a => Map Text [BzoFileModel a] -> [[BzoFileModel a]] -> [[BzoFileModel a]] -> Either [BzoErr] [[BzoFileModel a]]
orderByLinks mp out [] = Right out
orderByLinks mp out fs =
  let (remain, next) = L.break (\x -> containsManyMembers mp $ getLinkDependencies x) fs
      next'          = L.map (\x -> (bfm_domain $ L.head x, x)) next
  in case next of
    [] -> Left [CfgErr $ pack "Unsatisfiable Dependencies between libraries! Compilation cannot continue.\n"]
    nx -> orderByLinks (insertMany mp next') (out ++ next) remain









orderFileData :: Show a => [BzoFileModel a] -> Either [BzoErr] [BzoFileModel a]
orderFileData fs =
  let f0 = groupWith bfm_domain fs
      f1 = L.map (orderByImports M.empty []) f0
      f2 = L.concat $ lefts  f1
      f3 = [orderByLinks M.empty [] $ rights f1]
      f4 = lefts  f3
      f5 = L.concat $ rights f3
  in case (f2, f4) of
      ([], []) -> Right $ L.concat f5
      ([], er) -> Left  $ L.concat er
      (er, _ ) -> Left  er
