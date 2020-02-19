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

module Error where
import Data.Text as T










data BzoErr = Other
  | StringErr { position::BzoPos, errorStr::T.Text }
  | LexErr    { position::BzoPos, errorStr::T.Text }
  | ParseErr  { position::BzoPos, errorStr::T.Text }
  | TypeErr   { position::BzoPos, errorStr::T.Text }
  | SntxErr   { position::BzoPos, errorStr::T.Text }
  | DepErr    { errorStr::T.Text }
  | ParamErr  { errorStr::T.Text }
  | CfgErr    { errorStr::T.Text }
  | ModelErr  { position::BzoPos, errorStr::T.Text }
  | PrepErr   { position::BzoPos, errorStr::T.Text}










showBzErr :: BzoErr -> String
showBzErr (StringErr  p st) = "Bzo Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (LexErr     p st) = "Lexer Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (ParseErr   p st) = "Parse Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (TypeErr    p st) = "Type Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (SntxErr    p st) = "Syntax Error:\n " ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (DepErr       st) = "Dependency Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (ParamErr     st) = "Parameter Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (CfgErr       st) = "Configuration Error:\n" ++ (T.unpack st) ++ "\n\n"
showBzErr (PrepErr    p st) = "Preprocessor Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
showBzErr (ModelErr   p st) = "Modeller Error:\n" ++ (showErrPos p) ++ (T.unpack st) ++ "\n\n"
instance Show BzoErr where show = showBzErr










showErrPos :: BzoPos -> String
showErrPos p = "In file \"" ++ (show $ fileName p) ++ "\", at line " ++ (show $ line p) ++ ", column " ++ (show $ column p) ++ " ::\n"










data BzoPos = BzoPos {
  line     :: !Int,
  column   :: !Int,
  fileName :: !T.Text }
  deriving Eq

compareBzoPos :: BzoPos -> BzoPos -> Ordering
compareBzoPos (BzoPos l0 c0 f0) (BzoPos l1 c1 f1)
  | (f0 == f1) && (l0 == l1) && (c0 == c1) = EQ
  | (f0 == f1) && (l0 == l1)               = compare c0 c1
  | (f0 == f1)                             = compare l0 l1
  | otherwise                              = compare f0 f1

instance Ord BzoPos where compare = compareBzoPos










showPos :: BzoPos -> String
showPos (BzoPos l c fname) = " [P:" ++ (show fname) ++ "@L:" ++ (show l) ++ ",C:" ++ (show c) ++ "] "
instance Show BzoPos where show = showPos
