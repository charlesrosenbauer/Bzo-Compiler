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

module BzoConfigParser where
import BzoParser
import BzoTypes
import Error
import Tokens
import AST
import Data.List as L
import Data.Text as T










cfgParserIter :: Text -> [CfgSyntax] -> [CfgSyntax] -> Either [BzoErr] CfgSyntax

-- | Nothing to Parse?

cfgParserIter fname [] [] = Left $ [ParseErr (BzoPos 1 1 fname) $ pack "Nothing to Parse?"]



-- | Simple reductions

cfgParserIter fname tokens ((LibParseItem _ (TkNil)):stk)            = cfgParserIter fname tokens stk

cfgParserIter fname tokens ((LibParseItem _ (TkNewline _))
                           :(LibParseItem _ (TkStr _ path))
                           :(LibParseItem p (TkTypeId _ name)):stk)  = cfgParserIter fname tokens ((LibLine p name $ unpack path):stk)

cfgParserIter fname tokens (l0@(LibLine _ _ _)
                           :l1@(LibLine p _ _):stk)                  = cfgParserIter fname tokens ((LibLines p (l0:l1:[])):stk)

cfgParserIter fname tokens (l0@(LibLine _ _ _)
                           :   (LibLines p ls):stk)                  = cfgParserIter fname tokens ((LibLines p (l0:ls)):stk)

cfgParserIter fname tokens (l0@(LibLine p _ _)
                           :(LibParseItem _ (TkNewline _)):stk)      = cfgParserIter fname tokens ((LibLines p [l0]):stk)


-- | Control Logic

cfgParserIter fname [] [item]        = Right item

cfgParserIter fname [] (s:stack)     = Left [ParseErr (cpos s) (pack "Parser could not consume entire file.\n")]

cfgParserIter fname (t:tokens) stack = cfgParserIter fname tokens (t:stack)










parseLibCfgFile :: Text -> [BzoToken] -> Either [BzoErr] CfgSyntax
parseLibCfgFile f tks =
  let tks' = L.map (\tk -> LibParseItem (spos tk) tk) tks
  in case (cfgParserIter f tks' []) of
      Left errs -> Left errs
      Right ast -> Right ast
