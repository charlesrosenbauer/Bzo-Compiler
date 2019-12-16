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

module BzoExprModeller where
import BzoTypes
import BzoEmulator
import HigherOrder
import Query
import AST
import Core
import Error
import Data.Text
import Data.List as L
import Unsafe.Coerce
import Data.Int
import Text.Printf










getFloatHex :: Double -> String
getFloatHex f =
  let bits :: Int64
      bits = unsafeCoerce f

  in  printf "%08x" bits










printFunction :: BzoSyntax -> String
printFunction (BzS_TypDef    _ _ _ _) = ""
printFunction (BzS_FnTypeDef _ _ _ _) = ""
printFunction (BzS_Int           _ i) = "i" ++ (show i) ++ " "
printFunction (BzS_Flt           _ f) = "f" ++ (getFloatHex f) ++ " "
printFunction (BzS_Nil             _) = "- "
printFunction (BzS_Str           _ s) = "\"" ++ (unpack s) ++ "\" "     -- May cause problems when string contains escape codes
--printFunction (BzS_TyId          _ t) =  -- get type id mapping
--printFunction (BzS_Id            _ f) =  -- determine if id is function or type. If function, account for overloading
--printFunction (BzS_MId           _ x) =  -- get var id mapping
printFunction (BzS_Wildcard        _) = "_ "
printFunction (BzS_Cmpd         _ xs) = "(x303 " ++ (L.concatMap printFunction xs) ++ ") "
printFunction (BzS_Poly         _ xs) = "(x304 " ++ (L.concatMap printFunction xs) ++ ") "
printFunction (BzS_Statement     _ x) = printFunction x
printFunction (BzS_Expr         _ xs) = "(x300 " ++ (L.concatMap printFunction xs) ++ ") "
