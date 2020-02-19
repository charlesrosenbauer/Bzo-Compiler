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

module Compiler where
import Data.Tuple
import Data.List as L
import Data.Text as T
import Data.Map.Strict as M
import BzoTypes
import BzoPreprocessor
import BzoParameterParser
import BzoFileSorter
import ModelRules
import BzoChecker
import HigherOrder
import BzoExprModeller
import Error
import Core
import Debug.Trace










compileFilePass :: BzoSettings -> IO ()
compileFilePass (BzoSettings imp lib flg opt pfx) =
  let valid = areFilesValid (L.map fst imp)
  in do
      lCfg  <- getLibraryCfgContents (BzoSettings imp lib flg opt pfx)
      files <- loadSourceFiles $ L.map (\(fp,txt) -> (fp, pack txt)) imp

      -- Load, preprocess

      defs       <- return $ processFiles files
      defs'      <- (fmap (applyWithErr orderFileData)) $ (fmap (applyWithErr wrappedVerifier)) $ (applyWithErrM (wrappedLibLoader lCfg)) defs
      defs''     <- return $ (applyRight getDefTable) $ (applyRight getDefs) $ (applyRight (L.map (\x -> (adjustModel x modelXForm)))) defs'
      checkedast <- return $ applyWithErr checkProgram defs''
      program    <- return $ applyWithErr modelProgram checkedast

      -- TODO: Type Checker

      putStrLn $ case valid of
                  Nothing -> showOutput {-$ trace (show namemaps)-} program
                  Just er -> show er










showOutput :: Show a => Either [BzoErr] a -> String
showOutput (Right outs) = show outs
showOutput (Left [err]) =
  "Compilation Failed. Errors:\n\n" ++ show err ++ "\n\n1 error total."
showOutput (Left  errs) =
  "Compilation Failed. Errors:\n\n" ++ L.concatMap (\x -> show x ++ "\n\n") (sortErrs $ errs) ++ "\n\n" ++ (show $ L.length errs) ++ " errors total."










sortErrs :: [BzoErr] -> [BzoErr]
sortErrs = sortBy (\a b->
  case (compare (fileName $ position a) (fileName $ position b)) of
    EQ -> case (compare (line $ position a) (line $ position b)) of
            EQ -> (compare (column $ position a) (column $ position b))
            x  -> x
    x  -> x )









replExpression :: (FilePath, Text) -> String
replExpression s = showOutput (({-(applyWithErr wrappedModellerMapREPL).-} (applyWithErr wrappedParserMap). wrappedLexerMap) [s])










parseExpression :: (Text, FilePath) -> String
parseExpression s = showOutput (((applyWithErr wrappedParserMap). wrappedLexerMap) [swap s])










lexExpression :: (Text, FilePath) -> String
lexExpression s = showOutput (wrappedLexerMap [swap s])
